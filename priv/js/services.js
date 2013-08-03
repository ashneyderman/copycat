'use strict';

new function(_) {

    var CopycatClient = new base2.Package(this, {
        name:    "CopycatClient",
        version: "1.0",
        exports: "Client"
    })

    function fetchUser(username, login_response) {
        if(login_response.msgtype != base2.CopycatMsg.MSGTYPE_LOGIN_RESP) {
            throw new TypeError("fetchUser is fetching the user out of login response JS that should have msgtype field value of " + base2.CopycatMsg.MSGTYPE_LOGIN_RESP)
        }
        return new base2.CopycatModel.User(username, login_response.session)
    }

    /**
     * Client is the container of all things related to the user session.
     */
    var Client = base2.Base.extend({
        comms_channel: undefined,
        $q           : undefined,
        $rootScope   : undefined,

        // object properties
        pending_acks : new base2.Map(),  // map containing messages that were sent and are in need of acknowledgement.
        active_convs : new base2.Map(),  // map that contains associations between usernames and conversation threads.
        directory    : undefined,        // directory
        active_user  : undefined,        // user currently logged into this session

        evt_listeners : undefined,       // some events/messages are broadcast to all that want to listen (disconnect/reconnect)

        // constructor
        constructor : function(ClientConfig, $q, $rootScope) {
            this.comms_channel = new base2.CopycatComms.CommsChannel(ClientConfig.wsUrl, this)
            this.$q = $q
            this.$rootScope = $rootScope
            this.evt_listeners = new base2.Map()
            this.$rootScope.disconnected = false;
        },

        // object methods
        login : function(username, password, callbacks) {
            var loginReq = new base2.CopycatMsg.LoginReq(username, password)
            var deferred = this.$q.defer()
            this.pending_acks.put(loginReq.msgkey, {
                deferred : deferred,
                scope : this.$rootScope
            })
            this.comms_channel.send(loginReq)
            var client = this
            deferred.promise.then(
                function(loginResp) {
                    client.active_user = fetchUser(username,loginResp)
                    client.directory = new base2.CopycatModel.Directory(client.active_user, loginResp.roster)
                    if(callbacks.successFN) {
                        callbacks.successFN(loginResp)
                    }
                },
                function(error) {
                    if(callbacks.failureFN) {
                        callbacks.failureFN(error)
                    }
                })
        },

        logout : function() {
            var logoutReq = new base2.CopycatMsg.LogoutReq()
            this.comms_channel.send(logoutReq)
            this.notify_all({type: 'logout'})
            this.cleanup()
        },

        notify_all : function(evt) {
            var wrapper = this
            this.evt_listeners.getKeys().forEach(function(key) {
                var evt_listener = wrapper.evt_listeners.get(key)
                try { evt_listener.on(evt) } catch (e) {
                    console.log("Error occurred while notifying of onMsgReceived listener '" + key + "'")
                    console.log(e.stack)
                }
            })
        },

        cleanup : function() {
            this.pending_acks = new base2.Map()
            this.active_convs = new base2.Map()
            this.directory    = undefined
            this.active_user = undefined
            this.evt_listeners = new base2.Map()
            this.$rootScope.disconnected = false;
        },

        activateConv : function(userOrUsername) {
            // 0. Find the user in the directory
            var user = (userOrUsername instanceof base2.CopycatModel.User) ? userOrUsername : this.directory.lookupByUsername(userOrUsername)

            // 1. Try to find an already existing conversation
            var eConv = this.active_convs.get(user.username)
            if(eConv) {
                eConv.focused = true
                return eConv
            }

            // 2. If not found create a new one
            var eConv = new base2.CopycatModel.Conversation(user, this)
            this.active_convs.put(user.username, eConv)

            return eConv
        },

        sendTextMsg : function(msg,uirefresh) {  // these messages we do not wait for response here. Hence, we fire and forget.
            var msgOnWire = new base2.CopycatMsg.ChatMsg(msg.from.username,msg.to.username,msg.text)
            if(msg.msgkey) {
                msgOnWire.msgkey = msg.msgkey // assign the old key to the message on wire
            } else {
                msg.msgkey = msgOnWire.msgkey // save the new key in the model message
                this.fetchUserConv(msg.to).add(msg)
            }
            this.comms_channel.send(msgOnWire)
            if(uirefresh) {
                this.$rootScope.$digest()
            }
        },

        fetchUserConv : function(user) {
            if(!user) {
                return null
            }
            return this.active_convs.get(user.username)
        },

        // Communications channel callbacks
        onChannelOpen : function(evt) {
            // TODO: not sure if we need to react here.
            console.log("Event Open: " + evt)
        },

        onChannelClose : function(evt) {
            console.log("Event Close: " + evt)
        },

        onChannelError : function(evt) {
            // TODO: not sure if we need to react here.
            console.log("Event Error: " + evt)
        },

        onChannelMessage: function(evt, msg) {
            var dPair = this.pending_acks.get(msg.msgkey)
            if(dPair != undefined) {
                this.pending_acks.remove(msg.msgkey)
                if(msg.msgtype == base2.CopycatMsg.MSGTYPE_ERROR) {
                    dPair.scope.$apply(function() { dPair.deferred.reject(msg) })
                } else {
                    dPair.scope.$apply(function() { dPair.deferred.resolve(msg) })
                }
            } else {
                if(msg.msgtype == base2.CopycatMsg.MSGTYPE_PRESENCE) {
                    var user = this.directory.lookupByUsername(msg.username)
                    if(user) {
                        user.presence = msg.presence
                        this.$rootScope.$digest()
                    } else {
                        console.log("Presence message will be ignored. Username: " + msg.username + "; presence: " +  msg.presence + " is not found in our local directory.")
                    }
                } else if(msg.msgtype == base2.CopycatMsg.MSGTYPE_CHAT_MSG) {
                    var from = this.directory.lookupByUsername(msg.from)
                    var to = this.active_user
                    var m = new base2.CopycatModel.Message(msg.text,base2.CopycatModel.MSGDIR_IN,from,to)
                    m.time = msg.msgtime
                    var conv = this.activateConv(msg.from)
                    conv.add(m)
                    this.$rootScope.$digest()

                    // TODO - alex on 6/19/13: for now this all we do. We get the message we send read receipt.
                    var client = this
                    setTimeout(function() {
                        var readReceipt = new base2.CopycatMsg.ReadReceipt(msg.msgkey, msg.from)
                        client.comms_channel.send(readReceipt)
                    }, 1000) // (Math.floor(Math.random() * 5) + 1) * 1000)
                } else if(msg.msgtype == base2.CopycatMsg.MSGTYPE_READ_RECEIPT) {
                    var msgKey = msg.rspkey
                    var client = this
                    this.active_convs.getKeys().forEach(function(key){
                        client.active_convs.get(key).markMessageAs(msgKey, base2.CopycatModel.MSGSTATUS_READ)
                    })
                    this.$rootScope.$digest()
                } else if(msg.msgtype == base2.CopycatMsg.MSGTYPE_DISCONNECTED) {
                    this.disconnected(msg.undelivered, msg.progress_msg)
                } else if(msg.msgtype == base2.CopycatMsg.MSGTYPE_RECONNECTED) {
                    this.reconnected()
                } else if(msg.msgtype == base2.CopycatMsg.MSGTYPE_RECONNECTING) {
                    this.reconnecting(msg.progress_msg)
                } else if(msg.msgtype == base2.CopycatMsg.MSGTYPE_FAILED_RECONNECT) {
                    this.$rootScope.logout()
                    this.$rootScope.$digest()
                } else if(msg.msgtype == base2.CopycatMsg.MSGTYPE_PING) {
                    this.comms_channel.send(new base2.CopycatMsg.Pong())
                } else {
                    console.log("Received message: " + msg.msgtype + " we do not process this message.")
                }
            }
        },

        disconnected : function(msgKeys, progressMsg) {
            this.doPauseAllBots()
            this.doMarkAllReturned(msgKeys)
            this.$rootScope.disconnected = true
            this.$rootScope.progress_msg = progressMsg
            this.$rootScope.$digest()
        },

        doPauseAllBots : function() {
            var client = this
            this.active_convs.getKeys().forEach(function(key){
                client.active_convs.get(key).pauseBot()
            })
        },

        doMarkAllReturned : function(msgKeys) {
            if(msgKeys && msgKeys.length > 0) {
                var client = this
                for(var idx in msgKeys) {
                    var msgKey = msgKeys[idx]
                    this.active_convs.getKeys().forEach(function(key){
                        client.active_convs.get(key).markMessageAs(msgKey, base2.CopycatModel.MSGSTATUS_RETURNED)
                    })
                }
                this.comms_channel.markReturned(msgKeys)
            }
        },

        reconnected : function() {
            console.log("Reconnected is called")
            this.doResendReturned()
            this.$rootScope.disconnected = false
            this.$rootScope.progress_msg = ""
            this.$rootScope.$digest()
        },

        doResendReturned : function() {
            this.comms_channel.resendReturned()
            var client = this
            this.active_convs.getKeys().forEach(function(key) {
                client.active_convs.get(key).unmarkAllReturned()
            })
        },

        reconnecting : function(progressMsg) {
            this.$rootScope.progress_msg = progressMsg
            this.$rootScope.$digest()
        },

        addEventListener : function(key, listener) {
            if(!this.isValidEvtListener(listener)) {
                console.log("Message listener with the key of " + key + " is invalid.")
                throw new TypeError("Message listener you passed is invalid.")
            }

            if(this.evt_listeners.get(key)) {
                console.log("New listener is replacing existing listener for the key.")
            }
            this.evt_listeners.put(key, listener)
        },

        isValidEvtListener : function(listener) {
            return angular.isFunction(listener.on)
        }
    })

    eval(this.exports)
}

angular.module('refClient')
    .factory('ClientConfig', function($location) {
        this.wsUrl = "ws://" + $location.host() + ":9900/wsocket"
        return this
    })
    .factory('Client', function($q,$rootScope,ClientConfig) {
        return new base2.CopycatClient.Client(ClientConfig,$q,$rootScope);
    })

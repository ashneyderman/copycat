'use strict'

// depends on model and msg

/**
 * TODO - alex on 6/19/13: we need to find a way to clear out the cache of sent messages.
 *
 */
new function(_) {
    var CopycatComms = new base2.Package(this, {
        name:    "CopycatComms",
        version: "1.0",
        exports: "CommsChannel"
    })

    /**
     * Civilized (O-O) version of the web socket
     */
    var CommsChannel = base2.Base.extend({
        raw_socket : undefined,
        msg_listeners : undefined,
        sent_messages : new base2.Collection(),

        constructor : function(url, msgListener) {
            if(!url) {
                throw new TypeError("When you create Communications Channel WebSocket URL has to be specified.")
            }

            this.msg_listeners = new base2.Map()
            if(msgListener) {
                if(msgListener.key && this.isValidMsgListener(msgListener.listener)) {
                    this.addListener(msgListener.key, msgListener.listener)
                } else {
                    this.addListener("default", msgListener)
                }
            } else {
                this.addListener("default", {
                    onChannelOpen    : function(evt)      { console.log("On open occurred "  + evt) },
                    onChannelClose   : function(evt)      { console.log("On close occurred " + evt) },
                    onChannelError   : function(evt)      { console.log("On error occurred " + evt) },
                    onChannelMessage : function(msg, evt) { console.log("Message received "  + evt) }
                })
            }

            this.raw_socket = new WebSocket(url)
            var wrapper = this

            this.raw_socket.onopen = function(evt) {
                wrapper.msg_listeners.getKeys().forEach(function(key) {
                    var listener = wrapper.msg_listeners.get(key)
                    try { listener.onChannelOpen(evt) } catch (e) { console.log("Error occurred while notifying of onSocketOpen listener '" + key + "'") }
                })
            }

            this.raw_socket.onclose = function(evt) {
                wrapper.msg_listeners.getKeys().forEach(function(key) {
                    var listener = wrapper.msg_listeners.get(key)
                    try { listener.onChannelClose(evt) } catch (e) { console.log("Error occurred while notifying of onSocketClose listener '" + key + "'") }
                })
            }

            this.raw_socket.onerror   = function(evt) {
                wrapper.msg_listeners.getKeys().forEach(function(key) {
                    var listener = wrapper.msg_listeners.get(key)
                    try { listener.onChannelError(evt) } catch (e) { console.log("Error occurred while notifying of onSocketError listener '" + key + "'") }
                })
            }

            this.raw_socket.onmessage = function(evt) {
                var msg = wrapper.parse(evt.data)
                wrapper.msg_listeners.getKeys().forEach(function(key) {
                    var listener = wrapper.msg_listeners.get(key)
                    try { listener.onChannelMessage(evt, msg) } catch (e) {
                        console.log("Error occurred while notifying of onMsgReceived listener '" + key + "'")
                        console.log(e.stack)
                    }
                })
            }

            this.sent_messages = new base2.Collection()
        },

        /**
         * Parses incoming data and constructs javascript object.
         *
         * @param data - json
         * @returns javascript object
         */
        parse : function(data) {
            // WARNING: dependency on angular here.
            return angular.fromJson(data)
        },

        /**
         * Will send a message of type descendant from base2.CopycatMsg.BaseMessage. Will ignore the rest.
         *
         * @param msg
         */
        send : function(msg) {
            if(msg instanceof base2.CopycatMsg.BaseMessage) {
                if(msg.resendable) {
                    this.sent_messages.put(msg.msgkey, msg)
                }
                this.raw_socket.send(msg.stringify())
            } else {
                console.log("There is no way to send this message out since it does not extend base2.CopycatMsg.BaseMessage.")
            }
        },

        markReturned : function(msgKeys) {
            for(var idx in msgKeys) {
                var msgKey = msgKeys[idx]
                var msg = this.sent_messages.get(msgKey)
                if(msg && msg.msgkey == msgKey) {
                    msg.returned = true
                }
            }
        },

        resendReturned : function() {
            var commsCh = this

            this.sent_messages.getValues().forEach(function(msg) {
                if(msg.returned) {
                    commsCh.send(msg)
                    msg.returned = false
                }
            })
        },

        /**
         * Adds listener that will be notified of messages and/or events as they happen
         * on this communication channel. Listeners are indexed by a key, so multiple listeners
         * of the same kind are not registered un-necessarily.
         *
         * Listener is an object that responds to the the following calls -
         *      onSocketOpen  : function(evt) ...,
         *      onSocketClose : function(evt) ...,
         *      onSocketError : function(evt) ...,
         *      onMsgReceived : function(msg, evt) ...
         *
         * @param key - listener key
         * @param listener - listener
         */
        addListener : function(key, listener) {
            if(!this.isValidMsgListener(listener)) {
                console.log("Message listener with the key of " + key + " is invalid.")
                throw new TypeError("Message listener you passed is invalid.")
            }

            if(this.msg_listeners.get(key)) {
                console.log("New listener is replacing existing listener for the key.")
            }
            this.msg_listeners.put(key, listener)
        },

        isValidMsgListener : function(listener) {
            return angular.isFunction(listener.onChannelMessage)
        }

    })

    eval(this.exports)
}

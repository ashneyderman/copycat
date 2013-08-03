'use strict'

new function(_) {
    var CopycatMsg = new base2.Package(this, {
        name:    "CopycatMsg",
        version: "1.0",
        exports: "" +
            "BaseMessage,LoginReq,LogoutReq,ChatMsg,ReadReceipt,Pong," +
            "MSGTYPE_LOGIN_REQ,MSGTYPE_LOGIN_RESP,MSGTYPE_LOGOUT_REQ," +
            "MSGTYPE_LOGOUT_RESP,MSGTYPE_CHAT_MSG,MSGTYPE_READ_RECEIPT," +
            "MSGTYPE_PRESENCE,MSGTYPE_DISCONNECTED,MSGTYPE_RECONNECTED," +
            "MSGTYPE_RECONNECTING,MSGTYPE_FAILED_RECONNECT,MSGTYPE_PING," +
            "MSGTYPE_PONG,MSGTYPE_ERROR"
    })

    var MSGTYPE_ERROR        = "error"
    var MSGTYPE_LOGIN_REQ    = "login_req"
    var MSGTYPE_LOGIN_RESP   = "login_resp"
    var MSGTYPE_LOGOUT_REQ   = "logout_req"
    var MSGTYPE_LOGOUT_RESP  = "logout_resp"
    var MSGTYPE_CHAT_MSG     = "chat_msg"
    var MSGTYPE_READ_RECEIPT = "read_receipt"
    var MSGTYPE_PRESENCE     = "presence"
    var MSGTYPE_DISCONNECTED = "disconnected"
    var MSGTYPE_RECONNECTED  = "reconnected"
    var MSGTYPE_RECONNECTING = "reconnecting"
    var MSGTYPE_FAILED_RECONNECT = "failed_reconnect"
    var MSGTYPE_PING = "ping"
    var MSGTYPE_PONG = "pong"

    var BaseMessage = base2.Base.extend({
        msgtype : undefined,
        msgkey  : undefined,
        msgtime : undefined,
        returned: false,
        resendable : true,

        constructor : function(msgtype) {
            this.msgtype = msgtype
            this.msgkey = this.genkey()
            this.msgtime = new Date()
            this.returned = false
            this.resendable = true
        },

        genkey : function() {
            return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
                var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8)
                return v.toString(16)
            })
        },

        stringifyProps : function() {
            return ["msgtype", "msgkey", "msgtime"]
        },

        stringify : function() {
            var props = this.stringifyProps()
            var obj = {}

            for(var i = 0; i < props.length; i++) {
                obj[props[i]] = this[props[i]]
            }

            return angular.toJson(obj, true)
        }
    })

    var LoginReq = BaseMessage.extend({
        username : undefined,
        password : undefined,
        device_id : undefined,

        constructor : function(username, password) {
            this.base(MSGTYPE_LOGIN_REQ)
            this.username = username
            this.password = password
            this.device_id = this.genkey()
        },

        stringifyProps : function() {
            return this.base().concat([
                "username", "password", "device_id"
            ])
        }
    })

    var LogoutReq = BaseMessage.extend({
        constructor : function() {
            this.base(MSGTYPE_LOGOUT_REQ)
        }
    })

    var ChatMsg = BaseMessage.extend({
        from    : undefined,
        to      : undefined,
        text    : undefined,

        constructor : function(from, to, text) {
            this.base(MSGTYPE_CHAT_MSG)
            this.from = from
            this.to = to
            this.text = text
        },

        stringifyProps : function() {
            return this.base().concat([
                "from", "to", "text"
            ])
        }
    })

    var ReadReceipt =  BaseMessage.extend({
        rspkey   : undefined,
        to       : undefined,

        constructor : function(rspkey, to) {
            this.base(MSGTYPE_READ_RECEIPT)
            this.rspkey = rspkey
            this.to = to
        },

        stringifyProps : function() {
            return this.base().concat([
                "rspkey", "to"
            ])
        }
    })

    var Pong =  BaseMessage.extend({
        constructor : function() {
            this.base(MSGTYPE_PONG)
            this.resendable = false
        }
    })

    eval(this.exports)
}
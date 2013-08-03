'use strict';

new function(_) {
    var CopycatModel = new base2.Package(this, {
        name:    "CopycatModel",
        version: "1.0",
        exports: "Directory,Conversation,Message,User," +
                 "PRES_ONLINE,PRES_OFFLINE,PRES_AWAY,PRES_BUSY," +
                 "MSGDIR_IN,MSGDIR_OUT,BOT_MESSAGE_FREQUENCY," +
                 "MSGSTATUS_QUEUED,MSGSTATUS_RETURNED,MSGSTATUS_READ"
    })

    var BOT_MESSAGE_FREQUENCY = 2000

    var PRES_ONLINE  = "online"
    var PRES_OFFLINE = "offline"
    var PRES_AWAY    = "away"
    var PRES_BUSY    = "busy"

    var MSGDIR_IN    = "in"
    var MSGDIR_OUT   = "out"

    var MSGSTATUS_QUEUED   = 'queued'
    var MSGSTATUS_RETURNED = 'returned'
    var MSGSTATUS_READ     = 'read'

    // marking interface that indicates that we should be able to send this model object over the wire
    var BaseWiredModel = base2.Base.extend({})

    var Directory = base2.Base.extend({
        people : new base2.Collection(),

        constructor : function(activeUser, roster) {
            if(roster && roster) {
                for(var idx in roster) {
                    var temp = roster[idx]
                    if(temp.username == activeUser.username) {
                        continue;
                    }
                    var eUser = this.people.get(temp.username)
                    var u = eUser || new base2.CopycatModel.User(null, temp)
                    if(!eUser) { this.people.put(u.username, u) }
                }
            }
        },

        lookupByUsername: function(username) {
            return this.people.get(username)
        }
    })

    var Conversation = base2.Base.extend({
        partner  : undefined,
        messages : [],
        bot      : undefined,
        Client   : undefined,

        constructor : function(user, Client) {
            this.partner = user
            this.messages = []
            this.bot = new ConversationBot(this,BOT_MESSAGE_FREQUENCY)
            this.Client = Client
        },

        add : function(message) {
            this.messages.splice(0,0,message)
        },

        markMessageAs : function(msgKey, status) {
            for(var idx in this.messages) {
                var msg = this.messages[idx]
                if(msg.msgkey == msgKey) {
                    msg.status = status
                }
            }
        },

        unmarkAllReturned : function() {
            for(var idx in this.messages) {
                var msg = this.messages[idx]
                if(msg.status == MSGSTATUS_RETURNED) {
                    msg.status = MSGSTATUS_QUEUED
                }
            }
        },

        sendAutogeneratedMsg : function(msgtext) {
            var msg = new Message(msgtext, MSGDIR_OUT, this.Client.active_user, this.partner)
            this.Client.sendTextMsg(msg,true)
        },

        pauseBot : function() {
            this.bot.pause()
        },

        startBot : function() {
            this.bot.start()
        }

    })

    var ConversationBot = base2.Base.extend({
        conversation : undefined,
        sequence : 0,
        delay : BOT_MESSAGE_FREQUENCY,
        running : false,
        pendingTimerRef : undefined,

        constructor : function(conv, delay) {
            this.conversation = conv
            this.sequence = 0
            this.delay = delay
            this.running = false
            this.pendingTimerRef = undefined
        },

        pause : function() {
            if(this.pendingTimerRef) {
                clearTimeout(this.pendingTimerRef)
            }
            this.running = false;
        },

        start : function() {
            if(this.pendingTimerRef) {
                clearTimeout(this.pendingTimerRef)
            }
            var convBot = this
            var fun = function() {
                convBot.sequence += 1;
                convBot.conversation.sendAutogeneratedMsg("Message number " + convBot.sequence)
                convBot.pendingTimerRef = setTimeout(fun, convBot.delay)
            }
            this.pendingTimerRef = setTimeout(fun, this.delay)
            this.running = true;
        }
    })

    var Message = BaseWiredModel.extend({
        msgkey          : undefined,
        msg_dir         : undefined,
        text            : undefined,
        from            : undefined,
        to              : undefined,
        time            : undefined,
        read_receipt_on : undefined,
        displayed       : false,
        status          : MSGSTATUS_QUEUED,

        constructor : function(text,msg_dir,from,to) {
            this.text = text
            this.msg_dir = msg_dir
            this.from = from
            this.to = to
            this.time = new Date()
            this.read_receipt_on = undefined
            this.displayed = false
            this.status = MSGSTATUS_QUEUED
        }
    })

    var User = BaseWiredModel.extend({
        username   : undefined,
        last_name  : undefined,
        first_name : undefined,
        title      : undefined,
        phone_num  : undefined,
        ext        : undefined,
        presence   : undefined,

        constructor : function(username, vPrototype) {
            this.username = username
            if(!vPrototype) { vPrototype = {} }
            this.copyFrom(vPrototype)
        },

        copyFrom : function(vPrototype) {
            if(vPrototype.username) { this.username = vPrototype.username }
            this.last_name = vPrototype.last_name
            this.first_name = vPrototype.first_name
            this.title = vPrototype.title
            this.phone_num = vPrototype.phone_num
            this.ext = vPrototype.ext
            this.presence = vPrototype.presence
        }
    })

    eval(this.exports)
}
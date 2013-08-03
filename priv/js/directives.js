'use strict'

angular.module('refClient')
    .controller('ConversationCtrl', function($scope, Client) {
        $scope.conv = $scope.area.conv
        $scope.$watch('area.conv', function(newVal, oldVal) {
            $scope.conv = newVal
            var f = function(n,o) {
                $scope.presence = n
            }

            if($scope.conv) {
                $scope.placeholder = "Type and hit send to text " + $scope.conv.partner.first_name
                $scope.msgtext = ""
                $scope.$watch('area.conv.partner.presence', f)
            }
        })

        $scope.close = function() {
            $scope.conv.pauseBot()
            $scope.$parent.close($scope.location)
        }

        $scope.startBot = function() {
            $scope.conv.startBot()
        }

        $scope.pauseBot = function() {
            $scope.conv.pauseBot()
        }

        $scope.send = function() {
            var msg = new base2.CopycatModel.Message($scope.msgtext, base2.CopycatModel.MSGDIR_OUT, $scope.user, $scope.conv.partner)
            Client.sendTextMsg(msg)
            $scope.msgtext = ""
        }
    })

angular.module('refClient')
    .directive("user", function() {
        return {
            restrict: "E",
            replace : true,
            scope : {
                converse : "&",
                user : "="
            },
            templateUrl : "views/userstatus_tpl.html",
            link : function(scope, el, attrs) {
                scope.pres_indicator = scope.user.presence
                scope.$watch("user.presence", function(newVal, oldVal) {
                    scope.pres_indicator = newVal
                })
            }
        }
    })
    .directive("conversation", function() {
        return {
            restrict: "E",
            replace : true,
            controller : 'ConversationCtrl',
            scope : {
                area : "=",
                user : "=",
                location : "@"
            },
            templateUrl : "views/conversation_tpl.html"
        }
    })
    .directive("message", function() {
        return {
            restrict: "E",
            scope : {
                msg  : "="
            },
            template :
                '<div ng-show="msg.msg_dir == \'in\'">' +
                '   <div class="chatBlock">' +
                '      <table width="100%" cellspacing="0" cellpadding="0">' +
                '         <tbody>' +
                '         <tr>' +
                '            <td class="nameBlock them">' +
                '               <p class="eliptical">{{msg.from.first_name}}</p>' +
                '            </td>' +
                '            <td class="messageBlock them"><p class="timeBlock">{{msg.time | date:\'HH:mm:ss\'}}</p><p class="msgText">{{msg.text}}</p></td>' +
                '         </tr>' +
                '         </tbody>' +
                '      </table>' +
                '   </div>' +
                '</div>' +
                '<div ng-show="msg.msg_dir == \'out\'">' +
                '   <div class="chatBlock">' +
                '      <table width="100%" cellspacing="0" cellpadding="0">' +
                '         <tbody>' +
                '         <tr>' +
                '            <td class="nameBlock me">' +
                '               <p class="eliptical">{{msg.from.first_name}}</p>' +
                '            </td>' +
                '            <td class="messageBlock me"><p class="timeBlock">{{msg.time | date:\'HH:mm:ss\'}}</p><p class="msgText {{msg_status}}">{{msg.text}}</p></td>' +
                '         </tr>' +
                '         </tbody>' +
                '      </table>' +
                '   </div>' +
                '</div>',
            link : function(scope, el, attrs) {
                scope.msg_status = scope.msg.status
                scope.$watch("msg.status", function(newVal, oldVal) {
                    scope.msg_status = newVal
                })
            }
        }
    })
'use strict';

angular.module('refClient')
    .controller('LoginCtrl', function($scope, $rootScope, $location, Client) {
        $scope.client = Client;

        $rootScope.logout = function() {
            $scope.message = "warning";
            $scope.warning = "Logging out ...";
            Client.logout();
            $scope.authenticated = false;
            $scope.message = "warning";
            $scope.warning = "You were successfully logged out.";
            $location.url('/login')
        };

        // TODO alex on 5/16/13: not sure why this does not work when I go directly to /login
        if($scope.client.active_user) {
            $location.url('/text');
            return;
        }

        $scope.message = "warning";
        $scope.warning = "Please, login to proceed.";
        $scope.error   = "";

        $scope.login = function() {
            console.log("Username: " + $scope.username + "; password: " + $scope.password);
            $scope.message = "warning";
            $scope.warning = "Logging in ...";

            var loginCallbacks = {
                successFN : function() {
                    $location.url('/text')
                },
                failureFN : function(error) {
                    console.log("Error occurred with error code: " + error.error_code);
                    $scope.message = "error";
                    $scope.error = error.error_message;
                    $scope.authenticated = false;
                }
            };

            Client.login($scope.username, $scope.password, loginCallbacks);
        };
    })
    .controller('TextCtrl', function($scope, $rootScope, $location, $routeParams, Client) {
        $scope.client   = Client
        $scope.session  = {}
        $scope.areas = {
            a : {
                conv : undefined,
                visible : false,
                name : 'Area A'
            },

            b : {
                conv : undefined,
                visible : false,
                name : 'Area B'
            }
        }

        $scope.activateConv = function(user) {
            var eConv = $scope.client.activateConv(user.username)

            if(!$scope.areas.a.conv) {
                $scope.areas.a.conv = eConv
                $scope.areas.a.visible = true
            } else if(!$scope.areas.b.conv) {
                $scope.areas.b.conv = eConv
                $scope.areas.b.visible = true
            }
        }

        $scope.close = function(convArea) {
            if(convArea == 'a') {
                $scope.areas.a.visible = false
                $scope.areas.a.conv = undefined
            } else if(convArea == 'b') {
                $scope.areas.b.visible = false
                $scope.areas.b.conv = undefined
            }
        }
    })

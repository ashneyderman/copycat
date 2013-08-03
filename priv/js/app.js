'use strict';

angular.module('refClient', ['ngResource'])
    .config(function($routeProvider) {
        $routeProvider.when('/login', {templateUrl: 'views/login.html', controller: 'LoginCtrl'});
        $routeProvider.when('/text',  {templateUrl: 'views/text.html', controller: 'TextCtrl'});
        $routeProvider.otherwise({redirectTo: '/login'});
    });

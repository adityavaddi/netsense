
/************************************** Default Controller **************************************/


defaultApp.controller('defaultController', function($scope , $http, breadcrumbs)  {

    // Defining app scope 

    $scope.app = {}

    $scope.disabled = undefined;

    $scope.enable = function() {
        $scope.disabled = false;
    };

    $scope.disable = function() {
        $scope.disabled = true;
    };

    // Defining breadcrumb scope 

    $scope.breadcrumbs = breadcrumbs;

    /*$http.get("scripts/data1.json").success(function (response) {
        $scope.members = response.fielddata;
    }); */

});

/************************************** End Default Controller **************************************/


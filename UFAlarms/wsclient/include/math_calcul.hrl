%% Automatically generated, do not edit
%% Generated by gpb_compile version 3.20.1 on {{2016,3,30},{10,23,27}}

-ifndef(math_calcul).
-define(math_calcul, true).

-define(math_calcul_gpb_version, "3.20.1").

-ifndef('AREAOFCIRCLE_PB_H').
-define('AREAOFCIRCLE_PB_H', true).
-record(areaofcircle,
        {name,                          % = 1, string
         radius,                        % = 2, int32
         area,                          % = 3, int32 (optional)
         unit                           % = 4, string (optional)
        }).
-endif.

-ifndef('AREAOFSQUARE_PB_H').
-define('AREAOFSQUARE_PB_H', true).
-record(areaofsquare,
        {name,                          % = 1, string
         side,                          % = 2, int32
         area,                          % = 3, int32 (optional)
         unit                           % = 4, string (optional)
        }).
-endif.

-ifndef('AREAOFRECTANGLE_PB_H').
-define('AREAOFRECTANGLE_PB_H', true).
-record(areaofrectangle,
        {name,                          % = 1, string
         height,                        % = 2, int32
         width,                         % = 3, int32
         area,                          % = 4, int32 (optional)
         unit                           % = 5, string (optional)
        }).
-endif.

-ifndef('FACTORIAL_PB_H').
-define('FACTORIAL_PB_H', true).
-record(factorial,
        {name,                          % = 1, string
         number,                        % = 2, int32
         result                         % = 3, int32 (optional)
        }).
-endif.

-endif.

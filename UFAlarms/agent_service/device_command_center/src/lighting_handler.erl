-module(lighting_handler).
-compile(export_all).

%ScheduleEvent(id = 2, event = UnodeProto1.CalendarEvent(seconds = 0, minutes = 51, hours = 2), 
	%mode = UnodeProto1.ScheduleMode(priority = 7, mask = 0x00000001, driver = 100, qualifiers = 0x00000000))
%ScheduleEvent(id = 3, event = UnodeProto1.CalendarEvent(seconds = 0, minutes = 46, hours = 12), 
	%mode = UnodeProto1.ScheduleMode(priority = 7, mask = 0x00000001, driver = 0, qualifiers = 0x00000000))
%ScheduleEvent(id = 4, event = UnodeProto1.CalendarEvent(seconds = 0, minutes = 30, hours = 5), 
	%mode = UnodeProto1.ScheduleMode(priority = 5, mask = 0x00000001, driver = 15, qualifiers = 0x00000000))
%ScheduleEvent(id = 5, event = UnodeProto1.CalendarEvent(seconds = 0, minutes = 1, hours = 13), 
	%mode = UnodeProto1.ScheduleMode(priority = 5, mask = 0x00000001, driver = 0, qualifiers = 0x00000080))
%ScheduleEvent(id = 0, event = UnodeProto1.CalendarEvent(seconds = 0, minutes = 51, hours = 2), 
	%mode = UnodeProto1.ScheduleMode(priority = 1, mask = 0x00000001, driver = 100, qualifiers = 0x00000004))
%ScheduleEvent(id = 1, event = UnodeProto1.CalendarEvent(seconds = 0, minutes = 46, hours = 12), 
	%mode = UnodeProto1.ScheduleMode(priority = 1, mask = 0x00000001, driver = 0, qualifiers = 0x00000004))
%ScheduleEvent(id = 2, event = UnodeProto1.CalendarEvent(seconds = 0, minutes = 51, hours = 2), 
	%mode = UnodeProto1.ScheduleMode(priority = 7, mask = 0x00000001, driver = 100, qualifiers = 0x00000000))
%ScheduleEvent(id = 3, event = UnodeProto1.CalendarEvent(seconds = 0, minutes = 46, hours = 12), 
	%mode = UnodeProto1.ScheduleMode(priority = 7, mask = 0x00000001, driver = 0, qualifiers = 0x00000000))
%ScheduleEvent(id = 4, event = UnodeProto1.CalendarEvent(seconds = 0, minutes = 30, hours = 5), 
	%mode = UnodeProto1.ScheduleMode(priority = 5, mask = 0x00000001, driver = 15, qualifiers = 0x00000000))
%ScheduleEvent(id = 5, event = UnodeProto1.CalendarEvent(seconds = 0, minutes = 1, hours = 13), 
	%mode = UnodeProto1.ScheduleMode(priority = 5, mask = 0x00000001, driver = 0, qualifiers = 0x00000080))


send_default_schedule_morning(_Nodeid) ->
  %Lightprofile = getProfile(Nodeid),
  %{Nodegroup, Nodeid, Intensity} = Lightprofile,
  #{<<"name">> =><<"LightingScheduleEvent">>, <<"sec">> => 0, <<"min">> => 10, 
  <<"hr">> => 6, <<"pri">>=> 1 , <<"mask">> => 1,<<"level">> => <<0>>, <<"qualifiers">> => 0}.


send_default_schedule_evening() ->
  %Lightprofile = getProfile(NS),
  %{Nodegroup, Nodeid, Intensity} = Lightprofile,
  #{<<"name">> =><<"LightingScheduleEvent">>, <<"sec">> => 0, <<"min">> => 30, 
  <<"hr">> => 17, <<"pri">>=> 1 , <<"mask">> => 1,<<"level">> => <<1>>, <<"qualifiers">> => 0}.


send_default_weekend_schedule() ->
	 #{<<"name">> =><<"LightingScheduleEvent">>, <<"sec">> => 0, <<"min">> => 0, 
  <<"hr">> => 2, <<"wday">> => 6, <<"pri">>=> 2 , <<"mask">> => 1,<<"level">> => <<0>>, <<"qualifiers">> => 0}.

  getTimeSpecs(_Nodeid) ->
  	Dailylightoff = [{{'*','*','*'}, {6,0,0}}],
    %% ligths on at 5:30pm daily.
    Dailylighton = [{{'*','*','*'}, {17,30,0}}],
    DeltaLight = [{{'*','*','*'}, {22,0,0}}],
    Fridaynightspecial = [{fri, {22, 0, 0}}],
    [Dailylighton,Dailylightoff,DeltaLight, Fridaynightspecial].
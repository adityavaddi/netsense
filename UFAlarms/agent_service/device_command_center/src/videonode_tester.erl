-module(videonode_tester).
-compile(export_all).
-compile([{parse_transform, lager_transform}]).


termtostring()->
A = #{"name"=>"gina","lname"=>"hagg"},
R= io_lib:format("~p",[A]) ,
lists:flatten(R).


get_vnode_id() ->
  #{"o" => "abc123","p" => "nodeid"}.

get_vnode_topic() ->
  "local/cfgsvc/get".

question() ->
  %M = get_vnode_id(),
  M = get_ssid(),
  T = <<"local/cfgsvc/get">>,
  P = msgpack:pack(M),
  {ok,D} = emqttc:start_link([{client_id, <<"gina">>},{host,"10.0.1.9"},{username,<<"sensity">>},{password,<<"kentspeed">>}]),
  emqttc:publish(D, T,P, [{qos, 0}]).

get_ssid() ->
  #{"0" => "abc123", "p" => "network.wlan-x.ssid"}.

subscribe() ->
  %T= <<"+/cfgsvc/get/#">>,
  T1 = <<"+/cfgsvc/notify/#">>,
  {ok,D} = emqttc:start_link([{client_id, <<"gina-sub">>},{host,"10.0.1.9"},{username,<<"sensity">>},{password,<<"kentspeed">>},{keepalive,300}]),
  emqttc:subscribe(D,T1).


get_cached() ->
#{"&u[QGPIKgAt1C.5Qr=tX" => ["A%{)e33Nxh.^QwS/V3U-"],"GFlQ28*B}P3SX4)SI({H" => ["0Jq+K$11TyU<AX3NB4.3"],"N1SNr[x&]-gdyYHGXKP(" => ["-#iSc2J)MAEYg3AV=3D*"],"Nw5A93QFHp}O/Y*m&b&O" => ["EV%r%YKwaplcgg=9Z.f}"],"SmXS^Jw9!KHCu*?LpwX#" => ["/]G>UvXN=:Pn{HLA=>aB"],"w/4%1@+LV6txZyZXzcL." => ["FH&Pe.*P)=<y7h)&PnS-"]}. 

get_now() ->
{"h=BcQueSxtq@1?BY/m)e",#{"&u[QGPIKgAt1C.5Qr=tX" => ["A%{)e33Nxh.^QwS/V3U-"],"GFlQ28*B}P3SX4)SI({H" => ["0Jq+K$11TyU<AX3NB4.3"],"N1SNr[x&]-gdyYHGXKP(" => ["-#iSc2J)MAEYg3AV=3D*"],"Nw5A93QFHp}O/Y*m&b&O" => ["EV%r%YKwaplcgg=9Z.f}"],"PSsQ5yugwcgM/ee+Np*Z" => ["D%42.T1@RU+S<K}XNJve"],"SmXS^Jw9!KHCu*?LpwX#" => ["/]G>UvXN=:Pn{HLA=>aB"],"w/4%1@+LV6txZyZXzcL." => ["FH&Pe.*P)=<y7h)&PnS-"]}}.


get_less() ->
  {"h=BcQueSxtq@1?BY/m)e",#{"GFlQ28*B}P3SX4)SI({H" => ["0Jq+K$11TyU<AX3NB4.3"],"N1SNr[x&]-gdyYHGXKP(" => ["-#iSc2J)MAEYg3AV=3D*"],"Nw5A93QFHp}O/Y*m&b&O" => ["EV%r%YKwaplcgg=9Z.f}"],"PSsQ5yugwcgM/ee+Np*Z" => ["D%42.T1@RU+S<K}XNJve"],"SmXS^Jw9!KHCu*?LpwX#" => ["/]G>UvXN=:Pn{HLA=>aB"],"w/4%1@+LV6txZyZXzcL." => ["FH&Pe.*P)=<y7h)&PnS-"]}}.

get_more() ->
  {"h=BcQueSxtq@1?BY/m)e",#{"GFlQ28*B}P44X4)SI({H" => ["0Jq+K$11XyU<AX3NB4.3"],"GFlQ28*B}P3SX4)SI({H" => ["0Jq+K$11TyU<AX3NB4.3"],"N1SNr[x&]-gdyYHGXKP(" => ["-#iSc2J)MAEYg3AV=3D*"],"Nw5A93QFHp}O/Y*m&b&O" => ["EV%r%YKwaplcgg=9Z.f}"],"PSsQ5yugwcgM/ee+Np*Z" => ["D%42.T1@RU+S<K}XNJve"],"SmXS^Jw9!KHCu*?LpwX#" => ["/]G>UvXN=:Pn{HLA=>aB"],"w/4%1@+LV6txZyZXzcL." => ["FH&Pe.*P)=<y7h)&PnS-"]}}.

get_id() ->
   {"N02c00073", "evt", "dpark", "h=BcQueSxtq@1?BY/m)e"}.

test_delta_same() ->
  dcc_video_msg_transformer:get_delta(get_id() , get_cached(), get_now()).

test_delta_less() ->
  dcc_video_msg_transformer:get_delta(get_id() , get_cached(), get_less()).

test_delta_more() ->
  dcc_video_msg_transformer:get_delta(get_id() , get_cached(), get_more()).



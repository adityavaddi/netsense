%% The purpose of this module is to log how the example certs where created,
%% it requires erl_make_certs found in the test directory.

%%https://github.com/Xeralux/netsense/platform/services/device-security/src/main/scala/netsense/device/security/tools/KeyTools.scala
%%https://github.com/Xeralux/netsense/platform/servers/device-server/src/main/scala/netsense/device/server/HttpDeviceConnections.scala

-module(make_certs).
-export([all/0, make_certs/0]).

make_certs() ->
    CaInfo = {CaCert, _} = erl_make_certs:make_cert([{key, dsa}]),
    {Cert, {Asn1Type, Der, _}} = erl_make_certs:make_cert([{key, dsa}, {issuer, CaInfo}]),
    {CaCert, Cert, {Asn1Type, Der}}.

all() ->
    LongTime = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(date())+15*365),
    Validity = {date(), LongTime},
    Subject = [{email, "support@sensity.com"},
	       {city, "Sunnyvale"},
	       {country, "US"},
	       {org, "Sensity Systems"},
	       {org_unit, "Network Support"}],

    RootCa = erl_make_certs:make_cert([{validity, Validity}, {subject, [{name, "erlangCA"}|Subject]}]),
    ImedCa = erl_make_certs:make_cert([{issuer, RootCa}, {validity, Validity}, 
				       {subject, [{name, "otpCA"}|Subject]}]),
    ClientCa = erl_make_certs:make_cert([{issuer, ImedCa}, {validity, Validity}, 
					 {subject, [{name, "client"}|Subject]}]),
    ServerCa = erl_make_certs:make_cert([{issuer, ImedCa}, {validity, Validity}, 
					 {subject, [{name, "server"}|Subject]}]),

    Root0  = filename:dirname(filename:dirname((code:which(?MODULE)))),
    Root   = filename:join([Root0, "priv"]),    file:make_dir(Root), 
    CaPath = filename:join([Root, "erlangCA"]), file:make_dir(CaPath), 
    IPath  = filename:join([Root, "otpCA"]),   file:make_dir(IPath), 
    CPath  = filename:join([Root, "client"]),  file:make_dir(CPath), 
    SPath  = filename:join([Root, "server"]),  file:make_dir(SPath), 

    erl_make_certs:write_pem(CaPath,"cert", RootCa),
    erl_make_certs:write_pem(IPath, "cert", ImedCa),

    {ok, CaBin0} = file:read_file(filename:join(CaPath, "cert.pem")),
    {ok, CaBin1} = file:read_file(filename:join(IPath, "cert.pem")),
    CaBin = <<CaBin0/binary, CaBin1/binary>>, 

    erl_make_certs:write_pem(CPath, "cert", ClientCa),
    ok = file:write_file(filename:join(CPath, "cacerts.pem"), CaBin),
    erl_make_certs:write_pem(SPath, "cert", ServerCa),
    ok = file:write_file(filename:join(SPath, "cacerts.pem"), CaBin),
    
    file:delete(filename:join(CaPath, "cert_key.pem")),
    file:delete(filename:join(IPath, "cert_key.pem")),
    file:rename(filename:join(CPath, "cert_key.pem"), filename:join(CPath, "key.pem")), 
    file:rename(filename:join(SPath, "cert_key.pem"), filename:join(SPath, "key.pem")), 
    ok.


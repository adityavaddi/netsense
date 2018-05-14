-module(firmware_handler).

-behavior(cowboy_http_handler).

-export([content_type/1, handle/2, init/3,
	 terminate/3]).

-ignore_xref([{init, 3}, {handle, 2}, {terminate, 3}]).

-compile([{parse_transform, lager_transform}]).

init(_Transport, Req, []) -> {ok, Req, undefined}.

content_type([]) -> other;
content_type([{{<<"text">>, <<"html">>, _}, _, _}
	      | _]) ->
    html;
content_type([{{<<"application">>, <<"xhtml+xml">>, _},
	       _, _}
	      | _]) ->
    html;
content_type([{{<<"application">>, <<"json">>, _}, _, _}
	      | _]) ->
    json;
content_type([{{<<"application">>, <<"msgpack">>, _}, _,
	       _}
	      | _]) ->
    msgpack;
content_type([{{<<"application">>, <<"x-msgpack">>, _},
	       _, _}
	      | _]) ->
    msgpack;
content_type([_ | R]) -> content_type(R).

handle(Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),
    handle_req(Method, Req1, State).

handle_req(<<"GET">>, Req, State) ->
    proceed_with_download(Req, State);
handle_req(_Other, Req, State) ->
    lager:info("only serving firmware download requests"),
    {ok, Req1} = cowboy_req:reply(200, [], [], Req),
    {ok, Req1, State}.

terminate(_Reason, _Req, State) -> {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
proceed_with_download(Req, State) ->
    {FwId, Req1} = cowboy_req:binding(fwid, Req),
    {NodeId, Req0} = cowboy_req:binding(node_id, Req),
    {Filename, Req1} = cowboy_req:binding(file, Req0),
    lager:info("trying to download firmware (~p) ~p "
	       "for node: ~p ",
	       [Filename, FwId, NodeId]),
    case get_firmware(erlang:binary_to_list(FwId),
		      erlang:binary_to_list(Filename))
	of
      {error, not_found} ->
	  lager:warning("sending file not found, will download it"),
      Fname = binary_to_list(Filename),
      DerivedFwId = case string:rstr(Fname, ".bin") > 0 of 
	       true -> Fname -- ".bin";
           _-> Fname -- ".ftfs"
      end,
	  Files = legacy_s3_server:download_firmware(DerivedFwId),
	  lager:debug("Files after download: ~p~n", [Files]),
	  case Files of
	    [] ->
		{ok, Req12} = cowboy_req:reply(404, Req1),
		{ok, Req12, State};
	    _ ->
		{ok,JustDownloadedFile} =
		    get_firmware(erlang:binary_to_list(FwId),
				 erlang:binary_to_list(Filename)),
		lager:debug("we downloaded the file ~p, will send it",
			    [JustDownloadedFile]),
		{ok, Req11} = transport_file(JustDownloadedFile, Req1),
		{ok, Req11, State}
	  end;
      {ok, File} ->
	  {ok, Req13} = transport_file(File, Req1),
	  {ok, Req13, State}
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
transport_file(File, Req1) ->
    Filesize = filelib:file_size(File),
    lager:info("sending file to the node: ~p", [File]),
    Fun = fun (Socket, Transport) ->
		  Transport:sendfile(Socket, File)
	  end,
    %gproc:send({p,l,ota_downloaded},{ota_downloaded, {NodeId, FwId, Filename}})
    Req2 = cowboy_req:set_resp_body_fun(Filesize, Fun,
					Req1),
    cowboy_req:reply(200, Req2).

get_firmware(FwId, Filename) ->
    FwDir = firmware_helper:firmware_directory(FwId),
    checkDirectory(FwDir, Filename).

checkDirectory([], _Filename) -> {error, not_found};
checkDirectory(FwDir, Filename) ->
    lager:debug("FwDir: ~p~n", [FwDir]),
    {ok, Files} = file:list_dir(FwDir),
    lager:debug("FwDir: ~p, Files: ~p~n", [FwDir, Files]),
    FilteredFiles = [V1
		     || V1 <- Files, checkDirectory_1(V1, Filename)],
    checkFiles(FilteredFiles, FwDir).

checkDirectory_1(File, Filename) ->
    case string:str(File, Filename) of
      0 -> false;
      _ -> true
    end.

checkFiles([], _FwDir) -> {error, not_found};
checkFiles([File | _], FwDir) ->
    {ok, string:join([FwDir, File], "/")}.

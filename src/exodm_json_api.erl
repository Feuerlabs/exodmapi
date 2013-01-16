%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Erlang library using JSON transport to access EXODM
%%% @end
%%% Created : 16 Jan 2013 by Tony Rogvall <tony@rogvall.se>

-module(exodm_json_api).

-export([
	 create_account/5,
	 list_accounts/1,
	 delete_account/1]).
-export([
	 create_yang_module/3,
	 list_yang_modules/2,
	 delete_yang_module/2]).
-export([
	 create_config_set/3,
	 list_config_sets/1,
	 delete_config_set/1,
	 add_config_set_members/2,
	 remove_config_set_members/2
	]).
-export([
	 create_device_type/2,
	 list_device_types/1,
	 delete_device_type/1
	]).
-export([
	 create_device_group/2,
	 list_device_groups/1,
	 delete_device_group/1,
	 add_device_group_members/2,
	 remove_device_group_members/2
	]).
-export([
	 create_device/5,
	 delete_device/3,
	 provision_device/4,
	 deprovision_devices/1,
	 list_devices/1,
	 list_device_group_members/2,
	 list_device_type_members/2,
	 list_config_set_members/2
	]).
-export([read_exodmrc/0,
	 read_exodmrc/1,
	 scan_exodmrc/1,
	 parse_exodmrc/1]).
	 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Admin account requests
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_account(Name, User, Mail, Passw, FullName) ->
    json_request("exodm_admin:create-account",
		 [{"name", Name},
		  {"admin-user", {array, [{struct, [{"uname", User},
						    {"email", Mail},
						    {"password", Passw},
						    {"fullname", FullName}]}]}}],
		 integer_to_list(random()),
		 admin).

list_accounts(N) when is_integer(N), N>=0 ->
    json_request("exodm_admin:list-accounts",
		 [{"n", N},
		 {"previous", ""}],
		 integer_to_list(random()),
		 admin,
		 "accounts").

delete_account(Name) ->
    json_request("exodm_admin:delete-account",
		 [{"name", Name}],
		 integer_to_list(random()),
		 admin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Yang modules
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_yang_module(Name, Repo, File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    json_request("exodm:create-yang-module",
			 [{"name", Name},
			  {"repository", Repo},
			  {"yang-module", Bin}],
			 integer_to_list(random()),
			 user);
	Err = {error, _Reason} ->
	    Err
    end.

list_yang_modules(N, system) when is_integer(N), N>=0 ->
    json_request("exodm:list-yang-modules",
		 [{"n", N},
		  {"repository", "system"},
		  {"previous", ""}],
		 integer_to_list(random()),
		 admin,
		 "yang-modules");
list_yang_modules(N, user) when is_integer(N), N>=0 ->
    json_request("exodm:list-yang-modules",
		 [{"n", N},
		  {"repository", "user"},
		  {"previous", ""}],
		 integer_to_list(random()),
		 user,
		 "yang-modules").

delete_yang_module(Name, Repo) ->
    json_request("exodm:delete-yang-module",
		 [{"name", Name},
		  {"repository", Repo}],
		 integer_to_list(random()),
		 user).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Config Set
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_config_set(Name, File, Url) ->
    json_request("exodm:create-config-set",
		 [{"name", Name},
		  {"yang", File},
		  {"notification-url", Url}],
		 integer_to_list(random()),
		 user).

list_config_sets(N) when is_integer(N), N>=0 ->
    json_request("exodm:list-config-sets",
		 [{"n", N},
		 {"previous", ""}],
		 integer_to_list(random()),
		 user,
		 "config-sets").

delete_config_set(Name) ->
    json_request("exodm:delete-config-set",
		 [{"name", Name}],
		 integer_to_list(random()),
		 user).

add_config_set_members(Types, IDs) ->
    json_request("exodm:add-config-set-members",
		 [{"name", {array,Types}}, 
		  {"dev-id", {array,IDs}}],
		 integer_to_list(random()),
		user).

remove_config_set_members(Types, IDs) ->
    json_request("exodm:remove-config-set-members",
		 [{"name", {array,Types}}, 
		  {"dev-id", {array,IDs}}],
		 integer_to_list(random()),
		user).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Device type
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_device_type(Type, Protocol) ->
    json_request("exodm:create-device-type",
		 [{"name", Type}, 
		  {"protocol", Protocol}],
		 integer_to_list(random()),
		 user).

list_device_types(N) when is_integer(N), N>=0 ->
    json_request("exodm:list-device-types",
		 [{"n", N},
		  {"previous", ""}],
		 integer_to_list(random()),
		 user,
		 "device-types").

delete_device_type(Type) ->
    json_request("exodm:delete-device-type",
		 [{"name", Type}],
		 integer_to_list(random()),
		user).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Device group
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_device_group(Name, Url) ->
    json_request("exodm:create-device-group",
		 [{"name", Name}, 
		  {"notification-url", Url}],
		 integer_to_list(random()),
		 user,
		 "gid").

list_device_groups(N) when is_integer(N), N>=0 ->
    json_request("exodm:list-device-groups",
		 [{"n", N},
		  {"previous", ""}],
		 integer_to_list(random()),
		 user,
		 "device-groups").

delete_device_group(GroupName) ->
    json_request("exodm:delete-device-group",
		 [{"name", GroupName}],
		 integer_to_list(random()),
		 user).

add_device_group_members(Groups,IDs) ->
    json_request("exodm:add-device-group-members",
		 [{"device-groups", {array,Groups}},
		  {"dev-id", {array,IDs}}],
		 integer_to_list(random()),
		 user).

remove_device_group_members(Groups,IDs) ->
    json_request("exodm:remove-device-group-members",
		 [{"device-groups", {array,Groups}}, 
		  {"dev-id", {array,IDs}}],
		 integer_to_list(random()),
		 user).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Device
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_device(Id, Type, ServerKey, DeviceKey, Group) ->
    provision_device(Id, Type, ServerKey, DeviceKey),
    add_config_set_members([Type], [Id]),
    add_device_group_members([Group],[Id]).

delete_device(Id, Type, Group) ->
    remove_device_group_members([Group],[Id]),
    remove_config_set_members([Type],[Id]),
    deprovision_devices([Id]).


provision_device(Id, Type, ServerKey, DeviceKey) ->
    json_request("exodm:provision-device",
		 [{"dev-id", Id},
		  {"device-type", Type},
		  {"server-key", ServerKey},
		  {"device-key", DeviceKey},
		  {"msisdn", "+467331231234"}],
		integer_to_list(random()),
		user).

deprovision_devices(IDs) ->
    json_request("exodm:deprovision-devices",
		 [{"dev-id", {array, IDs}}],
		 integer_to_list(random()),
		 user).    

list_devices(N) when is_integer(N), N>=0 ->
    json_request("exodm:list-devices",
		 [{"n", N},
		  {"previous", ""}],
		integer_to_list(random()),
		user,
		"devices").

list_device_group_members(Group, N) when is_integer(N), N>=0 ->
    json_request("exodm:list-device-group-members",
		 [{"name", Group},
		  {"n", N},
		  {"previous", ""}],
		integer_to_list(random()),
		user,
		"device-group-members").

list_device_type_members(Name, N) when is_integer(N), N>=0 ->
    json_request("exodm:list-device-type-members",
		 [{"name", Name},
		  {"n", N},
		  {"previous", ""}],
		integer_to_list(random()),
		user,
		"device-type-members").

list_config_set_members(Name, N) when is_integer(N), N>=0 ->
    json_request("exodm:list-config-set-members",
		 [{"name", Name},
		  {"n", N},
		  {"previous", ""}],
		integer_to_list(random()),
		user,
		"config-set-members").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% JSON API
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

json_request(Request, KeyValueList, TransId, User) ->
    json_request(Request, KeyValueList, TransId, User,0).

json_request(Request, KeyValueList, TransId, User, Result) ->
    JsonRequest = json_encode(Request, KeyValueList, TransId),
    {ok, {http_response, _Version, 200, _, _Header}, Data} = 
	http_post(JsonRequest, User),
    String = binary_to_list(Data),
    ct:pal("Json request ~p~n,result ~p",
	   [lists:flatten(JsonRequest), String]),
    {ok, {struct, Values}} = json2:decode_string(String),
    {"jsonrpc","2.0"} = lists:keyfind("jsonrpc",1,Values),
    {"id",TransId} = lists:keyfind("id",1,Values),
    if Result == 0 ->
	    {"result", {struct,[{"result",0}]}} = 
		lists:keyfind("result",1, Values),
	    ok;
       Result == "gid" ->
	    {"result",{struct,[{"result",0},{"gid",GroupId}]}} = 
		lists:keyfind("result",1, Values),
	    GroupId;
       true ->
	    {"result", {struct,[{Result,{array, List}}]}} =
		lists:keyfind("result",1, Values),
	    ct:pal("Got ~p", [List]),
	    List
    end.

json_encode(Request, KeyValueList, TransId) ->
    json2:encode({struct, [{"jsonrpc", "2.0"},
			   {"method", Request},
			   {"id", TransId},
			   {"params",
			    {struct, KeyValueList}}]}).

http_post(Request, UserType) ->
    Opts = read_exodmrc(),
    Url  = proplists:get_value(url, Opts),
    {User,Pass} = proplists:get_value(UserType,Opts),
    exo_http:wpost(Url,
		   [{'Content-Type', "application/json"}] ++ 
		       exo_http:make_headers(User,Pass),
		   iolist_to_binary(Request)).


random() ->
    %% Initialize
    random:seed(now()),
    %% Retreive
    random:uniform(16#1000000).

read_exodmrc() ->
    read_exodmrc(os:getenv("HOME")).

read_exodmrc(false) -> read_exodmrc(".");
read_exodmrc("") ->    read_exodmrc(".");
read_exodmrc(Dir) -> 
    case file:read_file(filename:join(Dir, ".exodmrc")) of
	{ok,Bin} -> parse_exodmrc(scan_exodmrc(Bin));
	Error -> Error
    end.

parse_exodmrc([[<<"URL">>,Url]|Ts]) ->
    [{url,binary_to_list(Url)} | parse_exodmrc(Ts)];
parse_exodmrc([[<<"USER_AUTH">>,Auth]|Ts]) ->
    [User,Pass] = binary:split(Auth, <<":">>),
    [{user,{binary_to_list(User),binary_to_list(Pass)}} | parse_exodmrc(Ts)];
parse_exodmrc([[<<"ADMIN_AUTH">>,Auth]|Ts]) ->
    [User,Pass] = binary:split(Auth, <<":">>),
    [{admin,{binary_to_list(User),binary_to_list(Pass)}} | parse_exodmrc(Ts)];
parse_exodmrc([[Var,_Value]|Ts]) ->
    io:format(".exodmrc: warning: Unknown variabel ~s\n", [Var]),
    parse_exodmrc(Ts);
parse_exodmrc([]) ->
    [].


%% scan a .exodmrc file
%% return [ [Var,Val..] ..]
scan_exodmrc(Bin) ->	    
    lists:foldr(
      fun(Line, Acc) ->
	      case binary:split(Line, <<"#">>) of
		  [<<>>|_] -> Acc;
		  [L | _] -> [binary:split(L,<<"=">>) | Acc]
	      end
      end, [],binary:split(Bin, <<"\n">>, [global])).

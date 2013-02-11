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
	 list_account_roles/2,
	 delete_account/1]).
-export([
	 create_yang_module/4,
	 create_yang_module/3,
	 list_yang_modules/3,
	 list_yang_modules/2,
	 delete_yang_module/3,
	 delete_yang_module/2,
         list_execution_permission/4,
         list_execution_permission/3]).
-export([
	 create_user/4,
	 list_users/1,
	 delete_user/1,
	 add_account_access/3,
	 list_account_users/2,
	 remove_account_access/3
	]).
-export([
	 create_config_set/4,
	 create_config_set/3,
	 list_config_sets/2,
	 list_config_sets/1,
	 delete_config_set/2,
	 delete_config_set/1,
	 add_config_set_members/3,
	 add_config_set_members/2,
	 remove_config_set_members/3,
	 remove_config_set_members/2
	]).
-export([
	 create_device_type/3,
	 create_device_type/2,
	 list_device_types/2,
	 list_device_types/1,
	 delete_device_type/2,
	 delete_device_type/1
	]).
-export([
	 create_device_group/3,
	 create_device_group/2,
	 list_device_groups/2,
	 list_device_groups/1,
	 delete_device_group/2,
	 delete_device_group/1,
	 add_device_group_members/3,
	 add_device_group_members/2,
	 remove_device_group_members/3,
	 remove_device_group_members/2
	]).
-export([
	 create_device/6,
	 create_device/5,
	 delete_device/4,
	 delete_device/3,
	 provision_device/5,
	 provision_device/4,
	 deprovision_devices/2,
	 deprovision_devices/1,
	 list_devices/2,
	 list_devices/1,
	 list_device_group_members/3,
	 list_device_group_members/2,
	 list_device_type_members/3,
	 list_device_type_members/2,
	 list_config_set_members/3,
	 list_config_set_members/2
	]).
-export([set_exodmrc_dir/1,
	 read_exodmrc/0,
	 read_exodmrc/1,
	 scan_exodmrc/1,
	 parse_exodmrc/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Admin account requests
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_account(Name, User, Mail, Passw, FullName) ->
    json_request("exodm:create-account",
		 [{"name", Name},
		  {"admin-user", {array, [{struct, [{"uname", User},
						    {"email", Mail},
						    {"password", Passw},
						    {"fullname", FullName}]}]}}],
		 integer_to_list(random()),
		 admin).

list_accounts(N) when is_integer(N), N>=0 ->
    json_request("exodm:list-accounts",
		 [{"n", N},
		  {"previous", ""}],
		 integer_to_list(random()),
		 admin).

delete_account(Name) ->
    json_request("exodm:delete-account",
		 [{"name", Name}],
		 integer_to_list(random()),
		 admin).

list_account_roles(Account, N) when is_integer(N), N>=0 ->
    json_request("exodm:list-account-roles",
		 [{"account", Account},
                  {"n", N},
		  {"previous", ""}],
		 integer_to_list(random()),
		 admin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Users requests
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_user(User, Mail, Passw, FullName) ->
    json_request("exodm:create-user",
		 [{"uname", User},
		  {"email", Mail},
		  {"password", Passw},
		  {"fullname", FullName}],
		 integer_to_list(random()),
		 admin).

list_users(N) ->
    json_request("exodm:list-users",
		 [{"n", N},
		  {"previous", ""}],
		 integer_to_list(random()),
		 admin).

delete_user(Name) ->
    json_request("exodm:delete-user",
		 [{"uname", Name}],
		 integer_to_list(random()),
		 admin).

add_account_access(Account, Role, UserList) ->
    json_request("exodm:add-account-users",
		 [{"account", Account},
		  {"role", Role},
		  {"unames", {array, UserList}}],
		 integer_to_list(random()),
		 admin).

list_account_users(Account, N) ->
    json_request("exodm:list-account-users",
		 [{"account", Account},
		  {"n", N},
		  {"previous", ""}],
		 integer_to_list(random()),
		 admin).

remove_account_access(Account, Role, UserList) ->
    json_request("exodm:remove-account-users",
		 [{"account", Account},
		  {"role", Role},
		  {"unames", {array, UserList}}],
		 integer_to_list(random()),
		 admin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Yang modules
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_yang_module(Account, Name, Repo, File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    create_yang_module1([{"name", Name},
				 {"repository", Repo},
				 {"yang-module", Bin},
                                 {"account", Account}]);
	Err = {error, _Reason} ->
	    Err
    end.
create_yang_module(Name, Repo, File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    create_yang_module1([{"name", Name},
				 {"repository", Repo},
				 {"yang-module", Bin}]);
	Err = {error, _Reason} ->
	    Err
    end.
create_yang_module1(Params) when is_list(Params)->
    json_request("exodm:create-yang-module",
		 Params,
		 integer_to_list(random()),
		 user).


list_yang_modules(Account, N, system) when is_integer(N), N>=0 ->
    list_yang_modules1([{"n", N},
			{"repository", "system"},
			{"previous", ""},
                        {"account", Account}],
		       admin);
list_yang_modules(Account, N, user) when is_integer(N), N>=0 ->
    list_yang_modules1([{"n", N},
			{"repository", "user"},
			{"previous", ""},
                        {"account", Account}],
		       user).
list_yang_modules(N, system) when is_integer(N), N>=0 ->
    list_yang_modules1([{"n", N},
			{"repository", "system"},
			{"previous", ""}],
		       admin);
list_yang_modules(N, user) when is_integer(N), N>=0 ->
    list_yang_modules1([{"n", N},
			{"repository", "user"},
			{"previous", ""}],
		       user).
list_yang_modules1(Params, Client) when is_list(Params) ->
    json_request("exodm:list-yang-modules",
		 Params,
		 integer_to_list(random()),
		 Client).

delete_yang_module(Account, Name, Repo) ->
    delete_yang_module1([{"name", Name},
			 {"repository", Repo},
                         {"account", Account}]).
delete_yang_module(Name, Repo) ->
    delete_yang_module1([{"name", Name},
			 {"repository", Repo}]).
delete_yang_module1(Params) when is_list(Params)->
    json_request("exodm:delete-yang-module",
		 Params,
		 integer_to_list(random()),
		 user).

list_execution_permission(Account, system, Module, Rpc)  ->
    list_execution_permission1([{"repository", "system"},
                              {"modulename", Module},
                              {"rpcname", Rpc},
                              {"account", Account}],
                             admin);
list_execution_permission(Account, user, Module, Rpc)  ->
    list_execution_permission1([{"repository", "user"},
                              {"modulename", Module},
                              {"rpcname", Rpc},
                              {"account", Account}],
                             user).
list_execution_permission(system, Module, Rpc)  ->
    list_execution_permission1([{"repository", "system"},
                              {"modulename", Module},
                              {"rpcname", Rpc}],
                             admin);
list_execution_permission(user, Module, Rpc) ->
    list_execution_permission1([{"repository", "user"},
                              {"modulename", Module},
                              {"rpcname", Rpc}],
                             user).
list_execution_permission1(Params, Client) when is_list(Params) ->
    json_request("exodm:list-execution-permission",
		 Params,
		 integer_to_list(random()),
		 Client).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Config Set
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_config_set(Account, Name, File, Url) ->
    create_config_set1([{"name", Name},
			{"yang", File},
			{"notification-url", Url},
                        {"account", Account}]).
create_config_set(Name, File, Url) ->
    create_config_set1([{"name", Name},
			{"yang", File},
			{"notification-url", Url}]).
create_config_set1(Params) ->
    json_request("exodm:create-config-set",
		 Params,
		 integer_to_list(random()),
		 user).

list_config_sets(Account, N) when is_integer(N), N>=0 ->
    list_config_sets1([{"n", N},
		       {"previous", ""},
                       {"account", Account}]).
list_config_sets(N) when is_integer(N), N>=0 ->
    list_config_sets1([{"n", N},
		       {"previous", ""}]).
list_config_sets1(Params) when is_list(Params) ->
    json_request("exodm:list-config-sets",
		 Params,
		 integer_to_list(random()),
		 user).

delete_config_set(Account, Name) ->
    delete_config_set1([{"name", Name},
                        {"account", Account}]).
delete_config_set(Name) ->
    delete_config_set1([{"name", Name}]).
delete_config_set1(Params) ->
    json_request("exodm:delete-config-set",
		 Params,
		 integer_to_list(random()),
		 user).

add_config_set_members(Account, Types, IDs) ->
    add_config_set_members1([{"name", {array,Types}}, 
			     {"dev-id", {array,IDs}},
                             {"account", Account}]).
add_config_set_members(Types, IDs) ->
    add_config_set_members1([{"name", {array,Types}}, 
			     {"dev-id", {array,IDs}}]).
add_config_set_members1(Params) ->
    json_request("exodm:add-config-set-members",
		 Params,
		 integer_to_list(random()),
		 user).

remove_config_set_members(Account, Types, IDs) ->
    remove_config_set_members1([{"name", {array,Types}}, 
				{"dev-id", {array,IDs}},
                                {"account", Account}]).
remove_config_set_members(Types, IDs) ->
    remove_config_set_members1([{"name", {array,Types}}, 
				{"dev-id", {array,IDs}}]).
remove_config_set_members1(Params) ->
    json_request("exodm:remove-config-set-members",
		 Params,
		 integer_to_list(random()),
		 user).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Device type
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_device_type(Account, Type, Protocol) ->
    create_device_type1([{"name", Type}, 
			 {"protocol", Protocol},
                         {"account", Account}]).
create_device_type(Type, Protocol) ->
    create_device_type1([{"name", Type}, 
			 {"protocol", Protocol}]).
create_device_type1(Params) ->
    json_request("exodm:create-device-type",
		 Params,
		 integer_to_list(random()),
		 user).

list_device_types(Account, N) when is_integer(N), N>=0 ->
    list_device_types1([{"n", N},
			{"previous", ""},
                        {"account", Account}]).
list_device_types(N) when is_integer(N), N>=0 ->
    list_device_types1([{"n", N},
			{"previous", ""}]).
list_device_types1(Params) ->
    json_request("exodm:list-device-types",
		 Params,
		 integer_to_list(random()),
		 user).

delete_device_type(Account, Type) ->
    delete_device_type1([{"name", Type},
                         {"account", Account}]).
delete_device_type(Type) ->
    delete_device_type1([{"name", Type}]).
delete_device_type1(Params) ->
    json_request("exodm:delete-device-type",
		 Params,
		 integer_to_list(random()),
		 user).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Device group
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_device_group(Account, Name, Url) ->
    create_device_group1([{"name", Name}, 
			  {"notification-url", Url},
                          {"account", Account}]).
create_device_group(Name, Url) ->
    create_device_group1([{"name", Name}, 
			  {"notification-url", Url}]).
create_device_group1(Params) ->
    json_request("exodm:create-device-group",
		 Params,
		 integer_to_list(random()),
		 user).

list_device_groups(Account, N) when is_integer(N), N>=0 ->
    list_device_groups1([{"n", N},
			 {"previous", 0},
                         {"account", Account}]).
list_device_groups(N) when is_integer(N), N>=0 ->
    list_device_groups1([{"n", N},
			 {"previous", 0}]).
list_device_groups1(Params) ->
    json_request("exodm:list-device-groups",
		 Params,
		 integer_to_list(random()),
		 user).

delete_device_group(Account, GroupName) ->
    delete_device_group1([{"gid", GroupName},
                          {"account", Account}]).
delete_device_group(GroupName) ->
    delete_device_group1([{"gid", GroupName}]).
delete_device_group1(Params) ->
    json_request("exodm:delete-device-group",
		 %% [{"name", GroupName}], %% When delivered
		 Params, 
		 integer_to_list(random()),
		 user).

add_device_group_members(Account, Groups,IDs) ->
    add_device_group_members1([{"device-groups", {array,Groups}},
			       {"dev-id", {array,IDs}},
                               {"account", Account}]).
add_device_group_members(Groups,IDs) ->
    add_device_group_members1([{"device-groups", {array,Groups}},
			       {"dev-id", {array,IDs}}]).
add_device_group_members1(Params) ->
    json_request("exodm:add-device-group-members",
		 Params,
		 integer_to_list(random()),
		 user).

remove_device_group_members(Account, Groups,IDs) ->
    remove_device_group_members1([{"device-groups", {array,Groups}}, 
				  {"dev-id", {array,IDs}},
                                  {"account", Account}]).
remove_device_group_members(Groups,IDs) ->
    remove_device_group_members1([{"device-groups", {array,Groups}}, 
				  {"dev-id", {array,IDs}}]).
remove_device_group_members1(Params) ->
    json_request("exodm:remove-device-group-members",
		 Params,
		 integer_to_list(random()),
		 user).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Device
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_device(Account, Id, Type, ServerKey, DeviceKey, Group) ->
    provision_device(Account, Id, Type, ServerKey, DeviceKey),
    add_config_set_members(Account, [Type], [Id]),
    add_device_group_members(Account, [Group],[Id]).

create_device(Id, Type, ServerKey, DeviceKey, Group) ->
    provision_device(Id, Type, ServerKey, DeviceKey),
    add_config_set_members([Type], [Id]),
    add_device_group_members([Group],[Id]).

delete_device(Account, Id, Type, Group) ->
    remove_device_group_members(Account, [Group],[Id]),
    remove_config_set_members(Account, [Type],[Id]),
    deprovision_devices(Account, [Id]).

delete_device(Id, Type, Group) ->
    remove_device_group_members([Group],[Id]),
    remove_config_set_members([Type],[Id]),
    deprovision_devices([Id]).

provision_device(Account, Id, Type, ServerKey, DeviceKey) ->
    provision_device1([{"dev-id", Id},
		       {"device-type", Type},
		       {"server-key", ServerKey},
		       {"device-key", DeviceKey},
		       {"msisdn", "+467331231234"},
                       {"account", Account}]).
provision_device(Id, Type, ServerKey, DeviceKey) ->
    provision_device1([{"dev-id", Id},
		       {"device-type", Type},
		       {"server-key", ServerKey},
		       {"device-key", DeviceKey},
		       {"msisdn", "+467331231234"}]).
provision_device1(Params) ->
    json_request("exodm:provision-device",
		 Params,
		 integer_to_list(random()),
		 user).

deprovision_devices(Account, IDs) ->
    deprovision_devices1([{"dev-id", {array, IDs}},
                          {"account", Account}]).
deprovision_devices(IDs) ->
    deprovision_devices1([{"dev-id", {array, IDs}}]).
deprovision_devices1(Params) ->
    json_request("exodm:deprovision-devices",
		 Params,
		 integer_to_list(random()),
		 user).    

list_devices(Account, N) when is_integer(N), N>=0 ->
    list_devices1([{"n", N},
		   {"previous", ""},
                   {"account", Account}]).
list_devices(N) when is_integer(N), N>=0 ->
    list_devices1([{"n", N},
		   {"previous", ""}]).
list_devices1(Params) ->
    json_request("exodm:list-devices",
		 Params,
		 integer_to_list(random()),
		 user).

list_device_group_members(Account, Group, N) when is_integer(N), N>=0 ->
    list_device_group_members1([{"gid", Group},
				{"n", N},
				{"previous", ""},
                                {"account", Account}]).
list_device_group_members(Group, N) when is_integer(N), N>=0 ->
    list_device_group_members1([{"gid", Group},
				{"n", N},
				{"previous", ""}]).
list_device_group_members1(Params) ->
    json_request("exodm:list-device-group-members",
		 Params,
		 integer_to_list(random()),
		 user).

list_device_type_members(Account, Name, N) when is_integer(N), N>=0 ->
    list_device_type_members1([{"name", Name},
			       {"n", N},
			       {"previous", ""},
                               {"account", Account}]).
list_device_type_members(Name, N) when is_integer(N), N>=0 ->
    list_device_type_members1([{"name", Name},
			       {"n", N},
			       {"previous", ""}]).
list_device_type_members1(Params) ->
    json_request("exodm:list-device-type-members",
		 Params,
		 integer_to_list(random()),
		 user).

list_config_set_members(Account, Name, N) when is_integer(N), N>=0 ->
    list_config_set_members1([{"name", Name},
			      {"n", N},
			      {"previous", ""},
                              {"account", Account}]).
list_config_set_members(Name, N) when is_integer(N), N>=0 ->
    list_config_set_members1([{"name", Name},
			      {"n", N},
			      {"previous", ""}]).
list_config_set_members1(Params) ->
    json_request("exodm:list-config-set-members",
		 Params,
		 integer_to_list(random()),
		 user).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% JSON API
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

json_request(Request, KeyValueList, TransId, User) ->
    JsonRequest = json_encode(Request, KeyValueList, TransId),
    {ok, {http_response, _Version, 200, _, _Header}, Data} = 
	http_post(JsonRequest, User),
    String = binary_to_list(Data),
    ct:pal("Json request ~p~n,result ~p",
	   [lists:flatten(JsonRequest), String]),
    {ok, {struct, Values}} = json2:decode_string(String),
    {"jsonrpc","2.0"} = lists:keyfind("jsonrpc",1,Values),
    {"id",TransId} = lists:keyfind("id",1,Values),
    lists:keyfind("result",1, Values).

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

set_exodmrc_dir(Dir) ->
    %% Ugly solution :-(
    %% Find a better one ??
    %% Using application:set/get_env didn't seem to work :-(
    put(rcdir, Dir).

read_exodmrc() ->
    Dir = case get(rcdir) of
	      undefined -> os:getenv("HOME");
	      D-> D
	  end,
    read_exodmrc(Dir).

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

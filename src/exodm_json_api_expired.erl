%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Erlang library using JSON transport to access EXODM
%%% @end
%%% Created : 16 Jan 2013 by Tony Rogvall <tony@rogvall.se>

-module(exodm_json_api_expired).

-export([
	 create_account/4,
	 lookup_account/1,
	 list_accounts/2,
	 list_accounts/1,
	 list_account_roles/3,
	 list_account_roles/2,
	 delete_account/1]).
-export([
	 create_yang_module/4,
	 create_yang_module/3,
	 list_yang_modules/4,
	 list_yang_modules/3,
	 list_yang_modules/2,
	 lookup_yang_module/3,
	 lookup_yang_module/2,
	 delete_yang_module/3,
	 delete_yang_module/2,
         list_execution_permission/4,
         list_execution_permission/3]).
-export([
	 create_user/4,
	 list_users/1,
	 delete_user/1,
	 lookup_user/1,
	 add_account_access/3,
	 list_account_users/2,
	 list_account_users/3,
	 list_user_accounts/1,
	 remove_account_access/3
	]).
-export([
	 create_config_set/4,
	 create_config_set/3,
	 list_config_sets/3,
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
	 list_device_types/3,
	 list_device_types/2,
	 list_device_types/1,
	 delete_device_type/2,
	 delete_device_type/1
	]).
-export([
	 create_device_group/3,
	 create_device_group/2,
	 list_device_groups/3,
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
	 create_device/4,
	 delete_devices/2,
	 delete_devices/1,
	 list_devices/3,
	 list_devices/2,
	 list_devices/1,
	 list_device_group_members/4,
	 list_device_group_members/3,
	 list_device_group_members/2,
	 list_device_type_members/4,
	 list_device_type_members/3,
	 list_device_type_members/2,
	 list_config_set_members/4,
	 list_config_set_members/3,
	 list_config_set_members/2
	]).
-export([json_request/4]). %% Generic
-export([parse_result/2]).

-export([set_auth_method/1,
	 set_auth_option/2,
	 set_exodmrc_dir/1,
	 read_exodmrc/0,
	 read_exodmrc/1,
	 scan_exodmrc/1,
	 parse_exodmrc/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Admin account requests
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_account(Name, Mail, Passw, FullName) 
  when is_list(Name), is_list(Mail), is_list(Passw), is_list(FullName) ->
    json_request("exodm:create-account",
		 [{"name", Name},
                  {"email", Mail},
                  {"password", Passw},
                  {"fullname", FullName}],
		 integer_to_list(random()),
		 admin).

list_accounts(N) 
  when is_integer(N), N>=0 ->
    json_request("exodm:list-accounts",
		 [{"n", N},
		  {"previous", ""}],
		 integer_to_list(random()),
		 admin).

list_accounts(N, Prev) 
  when is_integer(N), N>=0, is_list(Prev) ->
    json_request("exodm:list-accounts",
		 [{"n", N},
		  {"previous", Prev}],
		 integer_to_list(random()),
		 admin).

delete_account(Name)
  when is_list(Name) ->
    json_request("exodm:delete-account",
		 [{"name", Name}],
		 integer_to_list(random()),
		 admin).

lookup_account(Name)
  when is_list(Name) ->
    json_request("exodm:lookup-account",
		 [{"name", Name}],
		 integer_to_list(random()),
		 admin).

list_account_roles(Account, N)
  when is_list(Account), is_integer(N), N>=0 ->
    json_request("exodm:list-account-roles",
		 [{"account", Account},
                  {"n", N},
		  {"previous", ""}],
		 integer_to_list(random()),
		 admin).

list_account_roles(Account, N, Prev) 
  when is_list(Account), is_integer(N), N>=0, is_list(Prev) ->
    json_request("exodm:list-account-roles",
		 [{"account", Account},
                  {"n", N},
		  {"previous", Prev}],
		 integer_to_list(random()),
		 admin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Users requests
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_user(Name, Mail, Passw, FullName) 
  when is_list(Name), is_list(Mail), is_list(Passw), is_list(FullName)->
    json_request("exodm:create-user",
		 [{"uname", Name},
		  {"email", Mail},
		  {"password", Passw},
		  {"fullname", FullName}],
		 integer_to_list(random()),
		 user).

list_users(N) 
  when is_integer(N), N>=0 ->
    json_request("exodm:list-users",
		 [{"n", N},
		  {"previous", ""}],
		 integer_to_list(random()),
		 user).

delete_user(Name) 
  when is_list(Name) ->
    json_request("exodm:delete-user",
		 [{"uname", Name}],
		 integer_to_list(random()),
		 admin).

lookup_user(Name) 
  when is_list(Name) ->
    json_request("exodm:lookup-user",
		 [{"uname", Name}],
		 integer_to_list(random()),
		 user).

add_account_access(Account, Role, UserList) 
  when is_list(Account), is_list(Role), is_list(UserList) ->
    json_request("exodm:add-account-users",
		 [{"account", Account},
		  {"role", Role},
		  {"unames", {array, UserList}}],
		 integer_to_list(random()),
		 user).

list_account_users(Account, N) 
  when is_list(Account), is_integer(N), N>=0 ->
    json_request("exodm:list-account-users",
		 [{"account", Account},
		  {"n", N},
		  {"previous", ""}],
		 integer_to_list(random()),
		 user).

list_account_users(Account, N, Prev) 
  when is_list(Account), is_integer(N), N>=0, is_list(Prev) ->
    json_request("exodm:list-account-users",
		 [{"account", Account},
		  {"n", N},
		  {"previous", Prev}],
		 integer_to_list(random()),
		 user).

list_user_accounts(Name) 
  when is_list(Name) ->
    json_request("exodm:list-user-accounts",
		 [{"uname", Name}],
		 integer_to_list(random()),
		 user).

remove_account_access(Account, Role, UserList) 
  when is_list(Account), is_list(Role), is_list(UserList) ->
    json_request("exodm:remove-account-users",
		 [{"account", Account},
		  {"role", Role},
		  {"unames", {array, UserList}}],
		 integer_to_list(random()),
		 user).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Yang modules
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_yang_module(Account, Name, Repo, File) 
  when is_list(Account), is_list(Name), is_list(Repo), is_list(File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    create_yang_module1([{"name", Name},
				 {"repository", Repo},
				 {"yang-module", Bin},
                                 {"account", Account}]);
	Err = {error, _Reason} ->
	    Err
    end.
create_yang_module(Name, Repo, File)  
  when is_list(Name), is_list(Repo), is_list(File) ->
    case file:read_file(File) of
	{ok, Bin} ->
	    create_yang_module1([{"repository", Repo},
				 {"name", Name},
				 {"yang-module", Bin}]);
	Err = {error, _Reason} ->
	    Err
    end.
create_yang_module1(Params) when is_list(Params)->
    json_request("exodm:create-yang-module",
		 Params,
		 integer_to_list(random()),
		 user).


list_yang_modules(Account, Repo, N, Prev) 
  when is_list(Account), is_list(Repo), is_integer(N), N>=0, is_list(Prev) ->
    list_yang_modules1([{"n", N},
			{"repository", Repo},
			{"previous", Prev},
                        {"account", Account}],
		       Repo).
list_yang_modules(Account, Repo, N) 
  when is_list(Account), is_list(Repo), is_integer(N), N>=0 ->
    list_yang_modules1([{"n", N},
			{"repository", Repo},
			{"previous", ""},
                        {"account", Account}],
		       Repo);
list_yang_modules(Repo, N, Prev) 
  when is_list(Repo), is_integer(N), N>=0, is_list(Prev) ->
    list_yang_modules1([{"n", N},
			{"repository", Repo},
			{"previous", Prev}],
		       Repo).
list_yang_modules(Repo, N) 
  when is_list(Repo), is_integer(N), N>=0 ->
    list_yang_modules1([{"n", N},
			{"repository", Repo},
			{"previous", ""}],
		       Repo).
list_yang_modules1(Params, Repo) when is_list(Params) ->
    json_request("exodm:list-yang-modules",
		 Params,
		 integer_to_list(random()),
		 client(Repo)).

lookup_yang_module(Account, Repo, Name) 
  when is_list(Account), is_list(Repo), is_list(Name) ->
    lookup_yang_module1([{"repository", Repo},
			{"name", Name},
                        {"account", Account}],
		       Repo).
lookup_yang_module(Repo, Name) 
  when is_list(Repo), is_list(Name) ->
    lookup_yang_module1([{"repository", Repo},
                         {"name", Name}],
                        Repo).
lookup_yang_module1(Params, Repo) when is_list(Params) ->
    json_request("exodm:lookup-yang-module",
		 Params,
		 integer_to_list(random()),
		 client(Repo)).


delete_yang_module(Account, Name, Repo) 
  when is_list(Account), is_list(Name), is_list(Repo) ->
    delete_yang_module1([{"name", Name},
			 {"repository", Repo},
                         {"account", Account}]).
delete_yang_module(Name, Repo) 
  when is_list(Name), is_list(Repo) ->
    delete_yang_module1([{"name", Name},
			 {"repository", Repo}]).
delete_yang_module1(Params) when is_list(Params)->
    json_request("exodm:delete-yang-module",
		 Params,
		 integer_to_list(random()),
		 user).

list_execution_permission(Account, Repo, Module, Rpc) 
  when is_list(Account), is_list(Module), is_list(Rpc) ->
    list_execution_permission1([{"repository", Repo},
                              {"modulename", Module},
                              {"rpcname", Rpc},
                              {"account", Account}],
                             Repo).
list_execution_permission(Repo, Module, Rpc) 
  when is_list(Module), is_list(Rpc) ->
    list_execution_permission1([{"repository", Repo},
                              {"modulename", Module},
                              {"rpcname", Rpc}],
                             Repo).
list_execution_permission1(Params, Repo) when is_list(Params) ->
    json_request("exodm:list-execution-permission",
		 Params,
		 integer_to_list(random()),
		 client(Repo)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Config Set
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_config_set(Account, Name, File, Url) 
  when is_list(Account), is_list(Name), is_list(File), is_list(Url) ->
    create_config_set1([{"name", Name},
			{"yang", File},
			{"notification-url", Url},
                        {"account", Account}]).
create_config_set(Name, File, Url)  
  when is_list(Name), is_list(File), is_list(Url) ->
    create_config_set1([{"name", Name},
			{"yang", File},
			{"notification-url", Url}]).
create_config_set1(Params) ->
    json_request("exodm:create-config-set",
		 Params,
		 integer_to_list(random()),
		 user).

list_config_sets(Account, N, Prev) 
  when is_list(Account), is_integer(N), N>=0, is_list(Prev) ->
    list_config_sets1([{"n", N},
		       {"previous", Prev},
                       {"account", Account}]).
list_config_sets(Account, N) 
  when is_list(Account), is_integer(N), N>=0 ->
    list_config_sets1([{"n", N},
		       {"previous", ""},
                       {"account", Account}]);
list_config_sets(N, Prev) 
  when is_integer(N), N>=0, is_list(Prev) ->
    list_config_sets1([{"n", N},
		       {"previous", Prev}]).
list_config_sets(N) 
  when is_integer(N), N>=0 ->
    list_config_sets1([{"n", N},
		       {"previous", ""}]).
list_config_sets1(Params) when is_list(Params) ->
    json_request("exodm:list-config-sets",
		 Params,
		 integer_to_list(random()),
		 user).

delete_config_set(Account, Name) 
  when is_list(Account), is_list(Name) ->
    delete_config_set1([{"name", Name},
                        {"account", Account}]).
delete_config_set(Name) 
  when is_list(Name) ->
    delete_config_set1([{"name", Name}]).
delete_config_set1(Params) ->
    json_request("exodm:delete-config-set",
		 Params,
		 integer_to_list(random()),
		 user).

add_config_set_members(Account, Types, IDs) 
  when is_list(Account), is_list(Types), is_list(IDs) ->
    add_config_set_members1([{"name", {array,Types}}, 
			     {"device-id", {array,IDs}},
                             {"account", Account}]).
add_config_set_members(Types, IDs)  
  when is_list(Types), is_list(IDs) ->
    add_config_set_members1([{"name", {array,Types}}, 
			     {"device-id", {array,IDs}}]).
add_config_set_members1(Params) ->
    json_request("exodm:add-config-set-members",
		 Params,
		 integer_to_list(random()),
		 user).

remove_config_set_members(Account, Types, IDs) 
  when is_list(Account), is_list(Types), is_list(IDs) ->
    remove_config_set_members1([{"name", {array,Types}}, 
				{"device-id", {array,IDs}},
                                {"account", Account}]).
remove_config_set_members(Types, IDs)   
  when is_list(Types), is_list(IDs) ->
    remove_config_set_members1([{"name", {array,Types}}, 
				{"device-id", {array,IDs}}]).
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

create_device_type(Account, Type, Protocol) 
  when is_list(Account), is_list(Type), is_list(Protocol) ->
    create_device_type1([{"name", Type}, 
			 {"protocol", Protocol},
                         {"account", Account}]).
create_device_type(Type, Protocol)  
  when is_list(Type), is_list(Protocol) ->
    create_device_type1([{"name", Type}, 
			 {"protocol", Protocol}]).
create_device_type1(Params) ->
    json_request("exodm:create-device-type",
		 Params,
		 integer_to_list(random()),
		 user).

list_device_types(Account, N, Prev) 
  when is_list(Account), is_integer(N), N>=0, is_list(Prev) ->
    list_device_types1([{"n", N},
			{"previous", Prev},
                        {"account", Account}]).
list_device_types(N, Prev) 
  when is_integer(N), N>=0, is_list(Prev) ->
    list_device_types1([{"n", N},
			{"previous", Prev}]);
list_device_types(Account, N) 
  when is_list(Account), is_integer(N), N>=0 ->
    list_device_types1([{"n", N},
			{"previous", ""},
                        {"account", Account}]).
list_device_types(N) 
  when is_integer(N), N>=0 ->
    list_device_types1([{"n", N},
			{"previous", ""}]).
list_device_types1(Params) ->
    json_request("exodm:list-device-types",
		 Params,
		 integer_to_list(random()),
		 user).

delete_device_type(Account, Type) 
  when is_list(Account), is_list(Type) ->
    delete_device_type1([{"name", Type},
                         {"account", Account}]).
delete_device_type(Type)  
  when is_list(Type) ->
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

create_device_group(Account, Name, Url) 
  when is_list(Account), is_list(Name), is_list(Url) ->
    create_device_group1([{"name", Name}, 
			  {"notification-url", Url},
                          {"account", Account}]).
create_device_group(Name, Url)  
  when is_list(Name), is_list(Url) ->
    create_device_group1([{"name", Name}, 
			  {"notification-url", Url}]).
create_device_group1(Params) ->
    json_request("exodm:create-device-group",
		 Params,
		 integer_to_list(random()),
		 user).

%% Change gaurds when using name istead of id!!!
list_device_groups(Account, N, Prev)
  when is_list(Account), is_integer(N), N>=0, is_list(Prev) ->
    list_device_groups1([{"n", N},
			 {"previous", Prev},
                         {"account", Account}]).
list_device_groups(N, Prev)
  when is_integer(N), N>=0, is_list(Prev) ->
    list_device_groups1([{"n", N},
			 {"previous", Prev}]);
list_device_groups(Account, N)
  when is_list(Account), is_integer(N), N>=0 ->
    list_device_groups1([{"n", N},
			 {"previous", ""},
                         {"account", Account}]).
list_device_groups(N)
  when is_integer(N), N>=0 ->
    list_device_groups1([{"n", N},
			 {"previous", ""}]).
list_device_groups1(Params) ->
    json_request("exodm:list-device-groups",
		 Params,
		 integer_to_list(random()),
		 user).

%% Change gaurds when using name istead of id!!!
delete_device_group(Account, GroupName)
  when is_list(Account), is_list(GroupName) ->
    delete_device_group1([{"group-id", GroupName},
                          {"account", Account}]).
delete_device_group(GroupName)
  when is_list(GroupName) ->
    delete_device_group1([{"group-id", GroupName}]).
delete_device_group1(Params) ->
    json_request("exodm:delete-device-group",
		 %% [{"name", GroupName}], %% When delivered
		 Params,
		 integer_to_list(random()),
		 user).

add_device_group_members(Account, Groups, IDs)
  when is_list(Account), is_list(Groups), is_list(IDs) ->
    add_device_group_members1([{"group-id", {array,Groups}},
			       {"device-id", {array,IDs}},
                               {"account", Account}]).
add_device_group_members(Groups,IDs)
  when is_list(Groups), is_list(IDs) ->
    add_device_group_members1([{"group-id", {array,Groups}},
			       {"device-id", {array,IDs}}]).
add_device_group_members1(Params) ->
    json_request("exodm:add-device-group-members",
		 Params,
		 integer_to_list(random()),
		 user).

remove_device_group_members(Account, Groups, IDs)
  when is_list(Account), is_list(Groups), is_list(IDs) ->
    remove_device_group_members1([{"group-id", {array,Groups}}, 
				  {"device-id", {array,IDs}},
                                  {"account", Account}]).
remove_device_group_members(Groups, IDs)   
  when is_list(Groups), is_list(IDs) ->
    remove_device_group_members1([{"group-id", {array,Groups}}, 
				  {"device-id", {array,IDs}}]).
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

create_device(Account, Id, Type, ServerKey, DeviceKey, MsIsdn) 
  when is_list(Account), is_list(Id), is_list(Type) ->
    create_device1([{"device-id", Id},
		    {"device-type", Type},
		    {"server-key", ServerKey},
		    {"device-key", DeviceKey},
		    {"msisdn", MsIsdn},
		    {"account", Account}]).
create_device(Account, Id, Type, ServerKey, DeviceKey) 
  when is_list(Account), is_list(Id), is_list(Type) ->
    create_device1([{"device-id", Id},
		    {"device-type", Type},
		    {"server-key", ServerKey},
		    {"device-key", DeviceKey},
		    {"msisdn", "+467331231234"},
		    {"account", Account}]).
create_device(Id, Type, ServerKey, DeviceKey) 
  when is_list(Id), is_list(Type)->
    create_device1([{"device-id", Id},
		    {"device-type", Type},
		    {"server-key", ServerKey},
		    {"device-key", DeviceKey},
		    {"msisdn", "+467331231234"}]).
create_device1(Params) ->
    json_request("exodm:create-device",
		 Params,
		 integer_to_list(random()),
		 user).

delete_devices(Account, IDs) 
  when is_list(Account), is_list(IDs) ->
    delete_devices1([{"device-id", {array, IDs}},
		     {"account", Account}]).
delete_devices(IDs) 
  when is_list(IDs) ->
    delete_devices1([{"device-id", {array, IDs}}]).
delete_devices1(Params) ->
    json_request("exodm:delete-devices",
		 Params,
		 integer_to_list(random()),
		 user).    

list_devices(Account, N, Prev) 
  when is_list(Account), is_integer(N), N>=0, is_list(Prev) ->
    list_devices1([{"n", N},
		   {"previous", Prev},
                   {"account", Account}]).
list_devices(N, Prev) 
  when is_integer(N), N>=0, is_list(Prev) ->
    list_devices1([{"n", N},
		   {"previous", Prev}]);
list_devices(Account, N) 
  when is_list(Account), is_integer(N), N>=0 ->
    list_devices1([{"n", N},
		   {"previous", ""},
                   {"account", Account}]).
list_devices(N) 
  when is_integer(N), N>=0 ->
    list_devices1([{"n", N},
		   {"previous", ""}]).
list_devices1(Params) ->
    json_request("exodm:list-devices",
		 Params,
		 integer_to_list(random()),
		 user).

%% Change gaurds when using name istead of id!!!
list_device_group_members(Account, Group, N, Prev) 
  when is_list(Account), is_list(Group), is_integer(N), N>=0, 
       is_list(Prev) ->
    list_device_group_members1([{"group-id", Group},
				{"n", N},
				{"previous", Prev},
                                {"account", Account}]).
list_device_group_members(Group, N, Prev) 
  when is_list(Group), is_integer(N), N>=0, is_list(Prev)->
    list_device_group_members1([{"group-id", Group},
				{"n", N},
				{"previous", Prev}]);
list_device_group_members(Account, Group, N) 
  when is_list(Account), is_list(Group), is_integer(N), N>=0 ->
    list_device_group_members1([{"group-id", Group},
				{"n", N},
				{"previous", ""},
                                {"account", Account}]).
list_device_group_members(Group, N) 
  when is_integer(Group), is_integer(N), N>=0 ->
    list_device_group_members1([{"group-id", Group},
				{"n", N},
				{"previous", ""}]).
list_device_group_members1(Params) ->
    json_request("exodm:list-device-group-members",
		 Params,
		 integer_to_list(random()),
		 user).

list_device_type_members(Account, Name, N, Prev) 
  when is_list(Account), is_list(Name), is_integer(N), N>=0, is_list(Prev) ->
    list_device_type_members1([{"name", Name},
			       {"n", N},
			       {"previous", Prev},
                               {"account", Account}]).
list_device_type_members(Name, N, Prev) 
  when is_list(Name), is_integer(N), N>=0, is_list(Prev) ->
    list_device_type_members1([{"name", Name},
			       {"n", N},
			       {"previous", ""}]);
list_device_type_members(Account, Name, N) 
  when is_list(Account), is_list(Name), is_integer(N), N>=0 ->
    list_device_type_members1([{"name", Name},
			       {"n", N},
			       {"previous", ""},
                               {"account", Account}]).
list_device_type_members(Name, N) 
  when is_list(Name), is_integer(N), N>=0 ->
    list_device_type_members1([{"name", Name},
			       {"n", N},
			       {"previous", ""}]).
list_device_type_members1(Params) ->
    json_request("exodm:list-device-type-members",
		 Params,
		 integer_to_list(random()),
		 user).

list_config_set_members(Account, Name, N, Prev) 
  when is_list(Account), is_list(Name), is_integer(N), N>=0, is_list(Prev) ->
    list_config_set_members1([{"name", Name},
			      {"n", N},
			      {"previous", Prev},
                              {"account", Account}]).
list_config_set_members(Name, N, Prev) 
  when is_list(Name), is_integer(N), N>=0, is_list(Prev) ->
    list_config_set_members1([{"name", Name},
			      {"n", N},
			      {"previous", Prev}]);
list_config_set_members(Account, Name, N) 
  when is_list(Account), is_list(Name), is_integer(N), N>=0 ->
    list_config_set_members1([{"name", Name},
			      {"n", N},
			      {"previous", ""},
                              {"account", Account}]).
list_config_set_members(Name, N) 
  when is_list(Name), is_integer(N), N>=0 ->
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
    case http_post(JsonRequest, User) of
	{ok, {http_response, _Version, 200, _, _Header}, Data} ->
	    String = binary_to_list(Data),
	    ct:pal("Json request ~p~n,result ~p",
		   [lists:flatten(JsonRequest), String]),
	    {ok, {struct, Values}} = json2:decode_string(String),
	    {"jsonrpc","2.0"} = lists:keyfind("jsonrpc",1,Values),
	    {"id",TransId} = lists:keyfind("id",1,Values),
	    lists:keyfind("result",1, Values);
	{ok, {http_response, _Version, 401, Reason, _Header}, _Data} ->
	    {error, Reason}
    end.

json_encode(Request, KeyValueList, TransId) ->
    json2:encode({struct, [{"jsonrpc", "2.0"},
			   {"method", Request},
			   {"id", TransId},
			   {"params",
			    {struct, KeyValueList}}]}).

http_post(Request, UserType) ->
    Opts = read_auth(),
    Url  = proplists:get_value(url, Opts),
    {User,Pass} = proplists:get_value(UserType,Opts),
    exo_http:wpost(Url,
		   [{'Content-Type', "application/json"}] ++ 
		       exo_http:make_headers(User,Pass),
		   iolist_to_binary(Request)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Parse json result
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_result(ResultStruct, "ok") ->
    %% Standard
    lager:debug("ok: result ~p",[ResultStruct]),
    {"result", {struct,[{"result", "ok"}]}} = ResultStruct,
    ok;
parse_result(ResultStruct, {item, Item}) ->
    lager:debug("{item , ~p}: result ~p",[Item, ResultStruct]),
    {"result",{struct,[{"result","ok"},{Item,Value}]}} = ResultStruct,
    Value;
parse_result(ResultStruct, {list, Items}) ->
    %% List result
    lager:debug("{list, ~p}: result ~p",[Items, ResultStruct]),
    {"result", {struct,[{Items,{array, List}}]}} = ResultStruct,
    List;
parse_result(ResultStruct, {lookup, Items}) ->
    %% Lookup functions, returns zero or one item ??
    lager:debug("{item_list , ~p}: result ~p",[Items, ResultStruct]),
    {"result",{struct,[{"result","ok"},{Items,{array, [{struct, Item}]}}]}} = 
        ResultStruct,
    lager:debug("{item_list , ~p}: item ~p",[Items, Item]),
    Item;
parse_result(ResultStruct, {error, Reason}) ->
    %% Expected error
    lager:debug("{error, ~p}: result ~p",[Reason, ResultStruct]),
    {"result",{struct,[{"result", Reason}]}} = ResultStruct,
    ok;
parse_result(ResultStruct, ok_or_error) ->
    %% Standard
    lager:debug("code: result ~p",[ResultStruct]),
    {"result", {struct,[{"result", Result}]}} = ResultStruct,
    Result;
parse_result({error, _Reason} = E, ok_or_error) ->
    %% Http error
    lager:debug("error: result ~p",[E]),
    E;
parse_result(_ResultStruct, any) ->
    %% Don't check result
    lager:debug("any: result ~p",[_ResultStruct]),
    ok;
parse_result(ResultStruct, _Other) ->
    %% Return everything
    lager:debug("~p: result ~p",[_Other,ResultStruct]),
    ResultStruct.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Autorization handling
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec set_auth_method(Method::exodmrc | process_dict) -> ok.
set_auth_method(Method) 
  when Method == exodmrc;
       Method == process_dict ->
    %% Ugly solution :-(
    %% Find a better one ??
    %% Using application:set/get_env didn't seem to work :-(
    put(method, Method).


read_auth() ->
    case get(method) of
	exodmrc -> read_exodmrc();
	undefined -> read_exodmrc(); %% Default
	process_dict -> read_process_dict()
    end.
		     
%% Using .exodmrc-file
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

%% Using process dictionary
set_auth_option(url, Value) 
  when is_list(Value) ->
    put(url, Value);
set_auth_option(user, {User, PassWord} = Value) 
  when is_list(User), is_list(PassWord) ->
    put(user, Value);
set_auth_option(admin, {Admin, PassWord} = Value) 
  when is_list(Admin), is_list(PassWord) ->
    put(admin, Value).

read_process_dict() ->
    [{Option, get(Option)} || Option <- [url, user, admin]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Util
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
random() ->
    %% Initialize
    random:seed(now()),
    %% Retreive
    random:uniform(16#1000000).


client("user") -> user;
client("system") -> admin. 
  

-define(load_nif_from_host_triple_dir(App, Lib, LoadInfo),
        case host_triple:native_lib_dirs(App) of
            {error, bad_name} ->
                AppS = atom_to_list(App),
                {error,
                 {load_failed,
                  "Could not determine the " ++ AppS ++" `priv` directory"}};
            Dirs ->
                lists:foldl(
                  fun
                      (_, ok) ->
                          ok;
                      (Dir, _) ->
                          logger:debug(
                            "~s: Trying to load \"~s\" from \"~s\"",
                            [App, Lib, Dir]),
                          Ret = erlang:load_nif(
                                  filename:join(Dir, Lib),
                                  LoadInfo),
                          case Ret of
                              ok ->
                                  logger:debug(
                                    "~s: Loaded \"~s\" successfully from \"~s\"",
                                    [App, Lib, Dir]);
                              _ ->
                                  logger:debug(
                                    "~s: Couldn't load \"~s\" from \"~s\":~n~p",
                                    [App, Lib, Dir, Ret])
                          end,
                          Ret
                  end, undefined, Dirs)
        end).

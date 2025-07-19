-module(iidx_mod).

-export([cli/0]).

-define(default_start_id, 32097).

%--- API -----------------------------------------------------------------------

cli() -> #{
    arguments => [
        #{
            name => iidx_folder,
            type => binary,
            help => "Path to the IIDX installation directory"
        },
        #{
            name => mod_name,
            type => binary,
            help => "Name of the mod to create"
        },
        #{
            name => mod_path,
            type => binary,
            default => ".",
            help => "Path where the mod will be installed (default: current directory)"
        },
        #{
            name => bms_folder,
            type => binary,
            default => ".",
            help => "Path to the folder containing all BMS songs"
        },
        #{
            name => start_id,
            short => $s,
            long => "-start-id",
            type => integer,
            default => ?default_start_id,
            help => "Starting song ID for the mod (default: 32097)"
        }
    ],
    help => "Creates a complete IIDX mod by converting all BMS songs and mixing them into music_data",
    handler => fun mod/1
}.

%--- Internals -----------------------------------------------------------------

mod(#{iidx_folder := IIDXFolder,
        mod_name := ModName,
        mod_path := ModPath,
        bms_folder := BMSFolder,
        start_id := StartID}) ->
    iidx_cli:info("Starting mod creation: ~s", [ModName]),
    iidx_cli:info("IIDX folder: ~s", [IIDXFolder]),
    iidx_cli:info("BMS folder: ~s", [BMSFolder]),
    iidx_cli:info("Mod path: ~s", [ModPath]),
    iidx_cli:info("Starting song ID: ~p", [StartID]),
    % 1. Read IIDX data from iidx_folder
    % 2. Find all BMS song folders in bms_folder
    % 3. Convert each BMS song to IIDX format
    % 4. Mix all songs into music_data
    % 5. Create mod directory structure
    % 6. Write all files to mod_path
    iidx_cli:assert_path_exists(IIDXFolder),
    iidx_cli:assert_path_exists(BMSFolder),
    ModFolder = filename:join(ModPath, ModName),
    {ok, Files} = file:list_dir(BMSFolder),
    Paths = [filename:join(BMSFolder, F) || F <- Files],
    BMSFolders = [P || P <- Paths, is_bms_folder(P)],
    NewSongIDs = gen_ids(length(BMSFolders), StartID),
    iidx_cli:info("Found BMS folders: ~p", [BMSFolders]),
    iidx_cli:info("Converting all BMS songs to IIDX format..."),
    convert_bms_songs(BMSFolders, NewSongIDs, ModFolder),
    iidx_cli:info("🥁 Mixing all songs into music_data..."),
    IIDXData = iidx_data:read_iidx_files(IIDXFolder),
    NewIIDXData = lists:foldl(
        fun({SongFolder, SongID}, DataAcc) ->
            iidx_mix:mix(DataAcc, SongFolder, SongID)
        end,
        IIDXData,
        lists:zip(BMSFolders, NewSongIDs)),
    iidx_data:write_iidx_files(NewIIDXData, ModFolder),
    iidx_cli:success("Done!"),
    iidx_cli:info("🕹️ Mod '~s' has been successfully blended into the game! 🔥", [ModName]),
    ok.

convert_bms_songs(BMSFolders, NewSongIDs, ModFolder) ->
    lists:foreach(
        fun({SongFolder, SongID}) ->
            SongFiles = iidx_convert:convert(SongFolder, SongID),
            iidx_data:write_iidx_files(SongFiles, ModFolder)
        end,
        lists:zip(BMSFolders, NewSongIDs)).

is_bms_folder(Path) ->
    FullWildcard = binary_to_list(filename:join(Path, "*.bms")),
    case filelib:wildcard(FullWildcard) of
        [] -> false;
        _ -> true
    end.

gen_ids(Count, StartID) ->
    lists:seq(StartID, StartID + Count - 1).

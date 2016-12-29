// include Fake lib
#r "packages/FAKE/tools/FakeLib.dll"
open Fake

// Properties
let solutionFileName = "./Soduku.sln"

let buildModeRelease = getBuildParamOrDefault "buildMode" "Release"
let buildModeDebug = getBuildParamOrDefault "buildMode" "Debug"

let currentPath = FileSystemHelper.currentDirectory

let releaseDir = "/bin/Release"
let debugDir = "/bin/Debug"

let setParams buildMode defaults =
        { defaults with
            Verbosity = Some(Quiet)
            Targets = ["Build"]
            Properties =
                [
                    "Optimize", "True"
                    "DebugSymbols", "True"
                    "Configuration", buildMode
                ]
         }


Target "Release" (fun _ -> 
    build (buildModeRelease |> setParams) solutionFileName |> DoNothing
)

Target "Default" (fun _ -> 
    build (buildModeDebug |> setParams) solutionFileName |> DoNothing
)

Target "Clean" (fun _ ->
    CleanDir ("./Soduku/" + debugDir)
    CleanDir ("./Soduku/" + releaseDir)
)

RunTargetOrDefault "Default"
# AppleScriptingBridge.jl

[![Build Status](https://github.com/jkrumbiegel/AppleScriptingBridge.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/jkrumbiegel/AppleScriptingBridge.jl/actions/workflows/CI.yml?query=branch%3Amain)

This experimental package uses [ObjectiveC.jl](https://github.com/JuliaInterop/ObjectiveC.jl) to call Apple's [ScriptingBridge](https://developer.apple.com/documentation/scriptingbridge) framework, which allows you to control applications like you normally would via AppleScript.
The package parses the XML scripting definition (sdef) file for a given application and creates a module with the appropriate classes as structs and commands as functions.
Enumerations are implemented using [EnumX.jl](https://github.com/fredrikekre/EnumX.jl/issues).

## Installation

Note that you currently need the `sdef` command line utility in your path, this comes with the XCode developer utilities.

```julia
]add https://github.com/jkrumbiegel/AppleScriptingBridge.jl
```

## Example

```julia
ASB.@generate_module_from_sdef PH "Photos"
ph = PH.application()
album = ph.albums[1]
@show album.name
favorites = filter(mi -> mi.favorite, album.mediaItems)
```

## Bugs

There are currently plenty of bugs and the implementation is still in flux.
It's currently easy to crash Julia if some automatically translated Objective C method signature is not quite correct, or because the interface specified in the sdef file is not really type safe.
Also, only those commands are added to an object as `obj.command(...)` if the sdef file documents the relationship.
However, many apps seem not to document all class which can be combined with a given command, and if you assume wrong, you get a crash.
Maybe future versions could do some reflection using Objective C to find out before calling if a given method is at all valid for a specific class.


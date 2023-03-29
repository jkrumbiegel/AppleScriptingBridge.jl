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
using AppleScriptingBridge
AppleScriptingBridge.@generate_module_from_sdef PH "Photos"
ph = PH.application()
album = ph.albums[1]
@show album.name
favorites = filter(mi -> mi.favorite, album.mediaItems)
```

You can read and write properties of objects via Julia's normal dot syntax.

```julia
n = album.name
album.name = uppercase(n)
```

Commands acting on objects can be accessed via dot syntax as well, so you can inspect options in the REPL by typing `someObject.<TAB>`.
There can be at most one positional argument (direct parameter) and an arbitrary number of keyword arguments (parameters) for a given command.
Auto-documentation is not perfect, yet, so consult Apple's script editor dictionary browser if you want to know more about the options.

```julia
someObject.someCommand(...)
```

## Bugs

There are currently plenty of bugs and the implementation is still in flux.
Be careful and try out workflows on test files first, before using it with anything important.
It's currently easy to crash Julia if some automatically translated Objective C method signature is not quite correct, or because the interface specified in the sdef file is not really type safe.
Also, only those commands are added to an object as `obj.command(...)` if the sdef file documents the relationship.
However, many apps seem not to document all class which can be combined with a given command.
You can try to manually call a command function with the target object as first argument, if you assume that it should work with it.
However, if you assume wrong, you will get a crash.
Maybe future versions could do some reflection using Objective C to find out before calling if a given method is at all valid for a specific class.

Also, many people say that ScriptingBridge is inherently buggy (it's also not really developed anymore).
I'm not saying this solution is perfect, but I dislike writing AppleScript so much that I had to try this way to automate some things away for myself, which could otherwise only be done using AppleScript or JXA (which is also not very well liked).
As I love having Julia's general purpose tools available for most other aspects of automation and I need only a small subset of AppleScript-like functionality, this tool is already useable and useful for me, but your mileage might vary (a lot) if you give it a try.

If you find more bugs, you are welcome to open issues, although I'll probably not have much time to fix them for you :)

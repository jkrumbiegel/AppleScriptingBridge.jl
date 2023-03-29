using AppleScriptingBridge
using Test

AppleScriptingBridge.@generate_module_from_sdef FI "Finder"
finder = FI.application()

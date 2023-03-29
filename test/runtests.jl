using AppleScriptingBridge
using Test

ASB.@generate_module_from_sdef FI "Finder"
finder = FI.application()

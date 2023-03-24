using AppleScriptingBridge
const ASB = AppleScriptingBridge
using ObjectiveC
using ObjectiveC.Foundation

using AppleScriptingBridge: SBApplication, SBElementArray

ASB.@generate_module_from_sdef CO "Capture One 23"
# ASB.@generate_module_from_sdef F "Finder"

bundleid = ASB.bundle_identifier("Capture One 23")
co = CO.Application(@objc [SBApplication applicationWithBundleIdentifier:bundleid::id{NSString}]::id{CO.Application})

doc = co.currentDocument
images = SBElementArray(@objc [doc::id{CO.Document} images]::id{SBElementArray})

reinterpret(CO.Image, images[1]).id

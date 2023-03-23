using AppleScriptingBridge
const ASB = AppleScriptingBridge
using ObjectiveC
using ObjectiveC.Foundation

using AppleScriptingBridge: SBApplication

ASB.@generate_module_from_sdef CO "/Applications/Capture One 23.app/Contents/Resources/CaptureOne.sdef"

bundleid = ASB.bundle_identifier("Capture One 23")
co = CO.Application(@objc [SBApplication applicationWithBundleIdentifier:bundleid::id{NSString}]::id{CO.Application})

doc = co.currentDocument
images = doc.variants
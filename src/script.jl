using AppleScriptingBridge
const ASB = AppleScriptingBridge
using ObjectiveC
using ObjectiveC.Foundation

using AppleScriptingBridge: SBApplication, SBElementArray

ObjectiveC.load_framework("ScriptingBridge")

ASB.@generate_module_from_sdef CO "Capture One 23"
# ASB.@generate_module_from_sdef F "Finder"

bundleid = ASB.bundle_identifier("Capture One 23")
co = CO.Application(@objc [SBApplication applicationWithBundleIdentifier:bundleid::id{NSString}]::id{CO.Application})

doc = co.currentDocument
images = doc.images

nsarr = NSArray(@objc [images::id{SBElementArray} get]::id{NSArray});



@time map(nsarr) do image
    reinterpret(CO.Image, image).name
end


filter(map(doc.collections) do collection
    co = reinterpret(CO.Collection, collection)
    (; co.name, co.kind, co.folder)
end) do co
    co.kind === CO.CollectionType.catalog_folder
end


module CO
    using EnumX
    using ObjectiveC
    using ObjectiveC.Foundation
    # that we need to import these is bad macro hygiene currently
    using AppleScriptingBridge: AppleScriptingBridge, SBElementArray, NSColor, NSPoint, SBObject

    include("tempfile.jl")
end
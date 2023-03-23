module AppleScriptingBridge

using EzXML: EzXML
using EnumX: EnumX
using ObjectiveC: load_framework, @objcwrapper, @objcproperties, id, NSString, NSInteger, NSURL, nil
using ObjectiveC.Foundation: Foundation


sdef(path) = parse_sdef(EzXML.readxml(path))

struct Typ
    type::String
    list::Bool
end

struct Result
    types::Vector{Typ}
    description::Union{Nothing,String}
end

EnumX.@enumx AccessorStyle index name id range relative test

function parse_accessor_style(s::String)
    s == "index" ?
    AccessorStyle.index :
    s == "name" ?
    AccessorStyle.name :
    s == "id" ?
    AccessorStyle.id :
    s == "range" ?
    AccessorStyle.range :
    s == "relative" ?
    AccessorStyle.relative :
    s == "test" ?
    AccessorStyle.test :
    error("Invalid accessor style $s")
end

struct Accessor
    style::AccessorStyle.T
end

EnumX.@enumx Access r w rw

function parse_access(s::String)
    s == "rw" ?
    Access.rw :
    s == "r" ?
    Access.r :
    s == "w" ?
    Access.w :
    error("Invalid access $s")
end

struct Element
    types::Vector{Typ}
    description::Union{Nothing,String}
    access::Access.T
    accessors::Vector{Accessor}
end

struct Property
    name::String
    code::String
    types::Vector{Typ}
    description::Union{Nothing,String}
    access::Access.T
end

struct Parameter
    name::String
    code::String
    types::Vector{Typ}
    description::Union{Nothing,String}
    optional::Bool
end

struct DirectParameter
    types::Vector{Typ}
    description::Union{Nothing,String}
end

struct Command
    name::String
    code::String
    description::Union{Nothing,String}
    result::Union{Nothing,Result}
    directparameter::Union{Nothing,DirectParameter}
    parameters::Vector{Parameter}
end

struct RespondsTo
    command::String
end

struct Class
    name::String
    code::String
    description::Union{Nothing,String}
    respondsto::Vector{RespondsTo}
    properties::Vector{Property}
    elements::Vector{Element}
end

struct Enumerator
    name::String
    code::String
    hidden::Bool
end

struct Enumeration
    name::String
    code::String
    enumerators::Vector{Enumerator}
end

struct Suite
    name::String
    code::String
    description::Union{Nothing,String}
    commands::Vector{Command}
    classes::Vector{Class}
    enumerations::Vector{Enumeration}
end

struct Dictionary
    title::String
    suites::Vector{Suite}
end

struct ValueType
    name::String
    code::String
end

struct Cocoa end

const TYPEDICT = Dict{String,Type}(
    "dictionary" => Dictionary,
    "suite" => Suite,
    "command" => Command,
    "class" => Class,
    "enumeration" => Enumeration,
    "enumerator" => Enumerator,
    "value-type" => ValueType,
    "cocoa" => Cocoa,
    "result" => Result,
    "property" => Property,
    "type" => Typ,
    "parameter" => Parameter,
    "direct-parameter" => DirectParameter,
    "responds-to" => RespondsTo,
    "element" => Element,
    "accessor" => Accessor,
)

function parse_node(node::EzXML.Node)
    T = get(TYPEDICT, node.name, nothing)
    if T === nothing
        @info "Unknown name: $(node.name)"
        return nothing
    end
    children::Vector{Any} = parse_node.(EzXML.eachelement(node))
    parse_node(T, node, children)
end

function parse_node(::Type{Dictionary}, node, children)
    Dictionary(
        node["title"],
        children,
    )
end

function extract!(T, vec)
    v::Vector{T} = filter(x -> x isa T, vec)
    filter!(x -> !(x isa T), vec)
    return v
end

getkey(node, key, default = nothing) = haskey(node, key) ? node[key] : default

function parse_node(::Type{Suite}, node, children)
    commands = extract!(Command, children)
    classes = extract!(Class, children)
    enumerations = extract!(Enumeration, children)
    Suite(
        node["name"],
        node["code"],
        getkey(node, "description"),
        commands,
        classes,
        enumerations,
    )
end

function parse_node(::Type{Command}, node, children)
    results = extract!(Result, children)
    result = isempty(results) ? nothing : only(results)
    directparameters = extract!(DirectParameter, children)
    directparameter = isempty(directparameters) ? nothing : only(directparameters)
    parameters = extract!(Parameter, children)
    Command(
        node["name"],
        node["code"],
        getkey(node, "description"),
        result,
        directparameter,
        parameters,
    )
end

function parse_node(::Type{Class}, node, children)
    respondsto = extract!(RespondsTo, children)
    properties = extract!(Property, children)
    elements = extract!(Element, children)
    Class(
        node["name"],
        node["code"],
        getkey(node, "description"),
        respondsto,
        properties,
        elements,
    )
end

function parse_node(::Type{Enumeration}, node, children)
    enumerators = extract!(Enumerator, children)
    Enumeration(
        node["name"],
        node["code"],
        enumerators,
    )
end

function parse_node(::Type{Enumerator}, node, children)
    Enumerator(
        node["name"],
        node["code"],
        getkey(node, "hidden") == "yes",
    )
end

function parse_node(::Type{ValueType}, node, children)
    ValueType(
        node["name"],
        node["code"],
    )
end

function parse_node(::Type{Cocoa}, node, children)
    # TODO
    # @show getproperty.(EzXML.attributes(node), :name)
    Cocoa()
end

function parse_node(::Type{Result}, node, children)
    types = haskey(node, "type") ? [Typ(node["type"], false)] : extract!(Typ, children)
    Result(
        types,
        getkey(node, "description")
    )
end

function parse_node(::Type{Property}, node, children)
    types = haskey(node, "type") ? [Typ(node["type"], false)] : extract!(Typ, children)
    Property(
        node["name"],
        node["code"],
        types,
        getkey(node, "description"),
        parse_access(getkey(node, "access", "rw"))
    )
end

function parse_node(::Type{Typ}, node, children)
    Typ(
        node["type"],
        getkey(node, "list") == "yes",
    )
end

function parse_node(::Type{Parameter}, node, children)
    types = haskey(node, "type") ? [Typ(node["type"], false)] : extract!(Typ, children)
    Parameter(
        node["name"],
        node["code"],
        types,
        getkey(node, "description"),
        getkey(node, "optional") == "yes",
    )
end

function parse_node(::Type{DirectParameter}, node, children)
    types = haskey(node, "type") ? [Typ(node["type"], false)] : extract!(Typ, children)
    DirectParameter(
        types,
        getkey(node, "description"),
    )
end

function parse_node(::Type{RespondsTo}, node, children)
    RespondsTo(
        haskey(node, "name") ? node["name"] : node["command"],
    )
end

function parse_node(::Type{Element}, node, children)
    types = haskey(node, "type") ? [Typ(node["type"], false)] : extract!(Typ, children)
    accessors = extract!(Accessor, children)
    Element(
        types,
        getkey(node, "description"),
        parse_access(getkey(node, "access", "rw")),
        accessors,
    )
end

function parse_node(::Type{Accessor}, node, children)
    Accessor(
        parse_accessor_style(getkey(node, "style", "rw"))
    )
end

function parse_sdef(xml)
    parse_node(xml.root)::Dictionary
end

function is_reserved_keyword(s::Symbol)
    s in [:baremodule, :begin, :break, :catch, :const, :continue, :do, :else, :elseif, :end, :export, :false, :finally, :for, :function, :global, :if, :import, :let, :local, :macro, :module, :quote, :return, :struct, :true, :try, :using, :while]
end

load_framework("ScriptingBridge")

# types currently missing in ObjectiveC.jl
@objcwrapper NSColor <: Foundation.NSObject
@objcwrapper NSPoint <: Foundation.NSObject
@objcwrapper NSDate <: Foundation.NSObject
@objcwrapper NSMutableArray <: Foundation.NSArray

# types needed for ScriptingBridge
@objcwrapper SBObject <: Foundation.NSObject
@objcwrapper SBApplication <: SBObject
@objcwrapper SBElementArray <: NSMutableArray

# any', `text', `integer',
# `real', `number', `boolean', `specifier', `location
# specifier', `record', `date', `file', `point', `rectangle',
# `type', or `missing value'
function translate_type(t, enumsyms)
    isenum = false

    ty = t == "text" ?
        NSString :
        t == "integer" ?
        NSInteger :
        t == "boolean" ?
        Bool :
        t == "real" ?
        Cdouble :
        t == "rectangle" ?
        Nothing : # TODO: NSRectangle?
        t == "file" ?
        NSURL :
        t == "RGB color" ?
        NSColor :
        t == "point" ?
        NSPoint :
        t == "date" ?
        NSDate :
        begin
            @info "Unhandled type $t"
            tt = transform_type_symbol(t)
            if tt in enumsyms
                isenum = true
                :($tt.T)
            else
                tt
            end
            # :(id{$()})
        end
    return ty, isenum
end

function generate_code(c::Class, enumsyms::Set{Symbol})
    n = split(c.name) .|> uppercasefirst |> join |> Symbol
    is_reserved_keyword(n) && (n = Symbol("_", n))

    function ispointertype(x)
        x ∉ (NSInteger, Bool, Cdouble)
    end

    props = map(c.properties) do prop
        na = lowercasefirst(join(uppercasefirst.(split(prop.name)))) |> Symbol # TODO: This will actually crash if it's not correct so _ prefix is bad
        is_reserved_keyword(na) && (na = Symbol("_", na))
        types = map(prop.types) do t
            ty, isenum = translate_type(t.type, enumsyms)
            t.list && (ty = :(id{SBElementArray})) # TODO: parametric type?
            (ty, isenum)
        end
        if length(types) == 1
            type, isenum = only(types)
            t = !ispointertype(type) || isenum ? type : :(id{$type})
        else
            all((type, isenum) -> ispointertype(type), types) || error("Not all pointer type: $(types)")
            t = :(id{Union{$(first.(types)...)}})
        end
        :(@autoproperty $na::$t)
    end

    objcode = :(@objcwrapper $n <: SBObject)

    propcode = :(
        @objcproperties $n begin
            $(props...)
        end
    )

    objcode, propcode
end

function generate_code(c::Command)
    n = join(split(c.name), "_") |> Symbol
    is_reserved_keyword(n) && (n = Symbol("_", n))

    quote
        @objcwrapper $n <: SBObject
    end
end

function transform_type_symbol(s)
    n = split(s) .|> uppercasefirst |> join |> Symbol
    is_reserved_keyword(n) && (n = Symbol("_", n))
    return n
end

function generate_code(e::Enumeration)
    n = transform_type_symbol(e.name)
    values = map(e.enumerators) do enu
        na = join(split(enu.name), "_") |> Symbol
        is_reserved_keyword(na) && (na = Symbol("_", na))

        # do the return code (four character) as Cint to make it suitable for enum
        chars = map(Cchar, reverse(collect(enu.code)))
        code_int32 = reinterpret(Cint, chars)[]

        :($na = $code_int32)
    end
    :(EnumX.@enumx $n $(values...))
end

function generate_code(d::Dictionary)
    enumcodes = []
    classcode_tuples = []

    for suite in d.suites
        append!(enumcodes, map(generate_code, suite.enumerations))
    end

    enumsyms = Set(map(enumcodes) do expr
        expr.args[3]::Symbol
    end)

    for suite in d.suites
        append!(classcode_tuples, map(c -> generate_code(c, enumsyms), suite.classes))
    end

    objcodes = getindex.(classcode_tuples, 1)
    propcodes = getindex.(classcode_tuples, 2)

    Expr(
        :block,
        Expr(:block, enumcodes...),
        Expr(:block, objcodes...),
        Expr(:block, propcodes...),
        # map(generate_code, s.commands)...,
    )
end

macro generate_module_from_sdef(namespace::Symbol, sdef_path)
    quote
        @eval module $namespace
            using EnumX
            using ObjectiveC
            using ObjectiveC.Foundation
            # that we need to import these is bad macro hygiene currently
            using AppleScriptingBridge: AppleScriptingBridge, SBElementArray, NSColor, NSPoint, SBObject

            dict = AppleScriptingBridge.sdef($sdef_path);

            ex = AppleScriptingBridge.generate_code(dict)
            display(ex)
            eval(ex)
        end
    end
end

function bundle_identifier(appname::String)::String
    command = "id of application \"$appname\""
    readchomp(`osascript -e $command`)
end

end

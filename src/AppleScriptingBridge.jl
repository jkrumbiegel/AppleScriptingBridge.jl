module AppleScriptingBridge

using EzXML: EzXML
using EnumX: EnumX
using ObjectiveC: load_framework, @objcwrapper, @objcproperties, id, NSString, NSInteger, NSURL, nil, NSNumber, NSObject
using ObjectiveC.Foundation: Foundation

sdef(appname) = parse_sdef(EzXML.parsexml(app_sdef(appname)))

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
    inherits::Union{Nothing,String}
end

struct ClassExtension
    extends::String
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
    classextensions::Vector{ClassExtension}
    enumerations::Vector{Enumeration}
    hidden::Bool
end

struct Dictionary
    title::Union{Nothing,String}
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
    "class-extension" => ClassExtension,
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
        getkey(node, "title"),
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
    classextensions = extract!(ClassExtension, children)
    enumerations = extract!(Enumeration, children)
    Suite(
        node["name"],
        node["code"],
        getkey(node, "description"),
        commands,
        classes,
        classextensions,
        enumerations,
        getkey(node, "hidden") == "yes",
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
        getkey(node, "inherits"),
    )
end

function parse_node(::Type{ClassExtension}, node, children)
    respondsto = extract!(RespondsTo, children)
    properties = extract!(Property, children)
    elements = extract!(Element, children)
    ClassExtension(
        node["extends"],
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
@objcwrapper NSRect <: Foundation.NSObject
@objcwrapper NSMutableArray <: Foundation.NSArray
@objcwrapper NSRecord <: Foundation.NSObject

# types needed for ScriptingBridge
@objcwrapper SBObject <: Foundation.NSObject
@objcwrapper SBApplication <: SBObject
@objcwrapper SBElementArray <: NSMutableArray

struct SBElementArrayWrapper{T<:SBObject} <: AbstractVector{T}
    x::SBElementArray
end

Base.size(s::SBElementArrayWrapper) = (s.x.count,)
function Base.getindex(s::SBElementArrayWrapper{T}, i::Integer) where T
    reinterpret(T, s.x[Int(i)])
end
Base.show(io::IO, s::SBElementArrayWrapper{T}) where T = print(io, "SBElementArrayWrapper{$T} with $(length(s)) elements")
Base.show(io::IO, ::MIME"text/plain", s::SBElementArrayWrapper{T}) where T = print(io, "SBElementArrayWrapper{$T} with $(length(s)) elements")

# any', `text', `integer',
# `real', `number', `boolean', `specifier', `location
# specifier', `record', `date', `file', `point', `rectangle',
# `type', or `missing value'
function translate_type(t, typedict)
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
        NSRect :
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

function get_name(prop::Property)
    na = lowercasefirst(join(uppercasefirst.(split(prop.name)))) |> Symbol
    is_reserved_keyword(na) && (na = Symbol("_", na)) # TODO: This will actually crash if it's not correct so _ prefix is bad
    return na
end

function get_name(res::RespondsTo)
    na = lowercasefirst(join(uppercasefirst.(split(res.command)))) |> Symbol
    is_reserved_keyword(na) && (na = Symbol("_", na)) # TODO: This will actually crash if it's not correct so _ prefix is bad
    return na
end

function get_name(el::Element)
    typ = only(el.types)
    @assert !typ.list
    na = lowercasefirst(join(uppercasefirst.(split(typ.type)))) * "s" |> Symbol
    is_reserved_keyword(na) && (na = Symbol("_", na)) # TODO: This will actually crash if it's not correct so _ prefix is bad
    return na
end

function get_type(p::Property, typedict)
    types = map(p.types) do t
        ty, isenum = translate_type(t.type, typedict)
        t.list && (ty = :(id{SBElementArray})) # TODO: parametric type?
        (ty, isenum)
    end

    ispointertype(x) = x ∉ (NSInteger, Bool, Cdouble)

    if length(types) == 1
        type, isenum = only(types)
        t = !ispointertype(type) || isenum ? type : :(id{$type})
    else
        all((type, isenum) -> ispointertype(type), types) || error("Not all pointer type: $(types)")
        t = :(id{Union{$(first.(types)...)}})
    end
    return t
end

function get_type(e::Element, typedict)
    :(id{SBElementArray}) # currently can't specify parametric type here
end

function extract_typeinfo(types::Vector{Typ}, typedict)
    if length(types) == 1
        type = only(types)
        typestring = type.type
        if !haskey(typedict, typestring)
            return nothing, nothing
        end
        typeexpr, isvaluetype = typedict[typestring]

        if type.list
            t = :(id{SBElementArray})
            return t, :(AppleScriptingBridge.SBElementArrayWrapper{$typeexpr})
        else
            t = isvaluetype ? typeexpr : :(id{$typeexpr})
            return t, nothing
        end
    else
        return :(id{SBElementArray}), nothing
    end
end

function make_propexpr(p::Property, typedict)
    name = get_name(p)
    directtype, conversionhint = extract_typeinfo(p.types, typedict)
    if directtype === nothing
        @warn "Skipping property `$(p.name)` because type $(only(p.types).type) is unknown."
        return []
    end
    exprs = []
    if p.access in (Access.r, Access.rw)
        push!(
            exprs,
            :(@getproperty $name function(obj)
                AppleScriptingBridge.convert_result(@objc([obj::id{SBObject} $name]::$directtype), $conversionhint)
            end)
        )
    end
    setmethodname = Symbol("set", uppercasefirst(String(name)))
    if p.access in (Access.w, Access.rw)
        push!(
            exprs,
            :(@setproperty! $name function(obj, value)
                @objc([obj::id{SBObject} $setmethodname:value::$directtype]::Nothing)
            end)
        )
    end
    return exprs
end

struct CommandFunc
    obj
    f::Function
end

(c::CommandFunc)(args...; kwargs...) = c.f(c.obj, args...; kwargs...)

command_symbol(command::String) = Symbol("cmd", join(uppercasefirst.(split(command))))

function make_propexpr(r::RespondsTo, typedict)
    name = get_name(r)
    cmdsym = command_symbol(r.command)
    ex = :(@getproperty $name function(obj)
        AppleScriptingBridge.CommandFunc(obj, $cmdsym)
    end)
    [ex] # for vcat
end

function make_propexpr(e::Element, typedict)
    name = get_name(e)
    eltype, isvaluetype  = typedict[only(e.types).type]
    @assert !isvaluetype # expect elements to be list of classes
    ex = :(@getproperty $name function(obj)
        AppleScriptingBridge.convert_result(@objc([obj::id{SBObject} $name]::id{SBElementArray}), AppleScriptingBridge.SBElementArrayWrapper{$eltype})
    end)
    [ex] # for vcat
end

to_classname(cname) = split(cname) .|> uppercasefirst |> join |> Symbol

function generate_code(c::Class, typedict::Dict, commanddict::Dict)
    n = to_classname(c.name)
    is_reserved_keyword(n) && (n = Symbol("_", n))

    propexprs = map(x -> make_propexpr(x, typedict), [c.properties; c.elements; c.respondsto])
    if !isempty(propexprs)
        propexprs = reduce(vcat, propexprs)
    end

    inheritance = c.inherits === nothing ||
        c.inherits == c.name ? # this seems to be possible
        SBObject : 
        to_classname(c.inherits)

    docstring = """
        $n

    $(c.description)

    ## Elements

    ```
    $(map(element_docs, c.elements)...)
    ```

    ## Properties

    ```
    $(map(prop_docs, c.properties)...)
    ```

    ## Responds To

    ```
    $(map(r -> respondsto_docs(r, commanddict), c.respondsto)...)
    ```
    """    

    objcode = quote
        @objcwrapper $n <: $inheritance
        @doc $docstring $n
    end

    propcode = isempty(propexprs) ? :() : :(
        @objcproperties $n begin
            $(propexprs...)
        end
    )

    objcode, propcode
end

function prop_docs(p::Property)
    "$(get_name(p)) ($(p.access))\n"
end

function element_docs(e::Element)
    "$(get_name(e)) ($(e.access))\n"
end

function respondsto_docs(r::RespondsTo, d::Dict{String,Command})
    c = d[r.command]
    "$(get_name(r))\n"
end

make_direct_parameter(::Nothing) = ()

function make_direct_parameter(d::DirectParameter)
    ()
end

function generate_code(c::Command, class_set::Set{String}, typedict)
    methodsym = Symbol(lowercasefirst(join(uppercasefirst.(split(c.name)))))
    
    # TODO
    if is_reserved_keyword(methodsym)
        return Expr(:block)
    end

    dp = c.directparameter
    params = c.parameters

    # this is all confusing as hell
    if dp === nothing
        pos_args = ()
        if isempty(params)
            method_args = (methodsym,)
            kw_args = ()
        else
            method_args = (methodsym,)
            kw_args = ()
        end
    elseif all(t -> t.type in class_set, dp.types)
        pos_args = ()
        # now we can leave out the direct parameter it seems
        # but if there's at least one parameter, the name of the first parameter
        # is included in the method name now
        if isempty(params)
            kw_args = ()
            method_args = (methodsym,)
        else
            kw_args, param_method_args = convert_parameters(methodsym, params, typedict)
            method_args = param_method_args
        end
    else
        Parameter
        kw_args, param_method_args = convert_parameters(params, typedict)
        pos_args = (:direct,)
        method_args = (:($methodsym:direct::id{NSObject}), param_method_args...)
        # in this case we need the direct parameter explicitly
        # but parameter names don't influence method name
    end

    returntype = Nothing
    description = c.description === nothing ? "No description" : c.description

    direct_param = if c.directparameter === nothing
        "No direct parameter"
    else
        dp = c.directparameter
        """
        Description: $(dp.description === nothing ? "No description available." : dp.description)
        $(map(dp.types) do type
            # "- $(type.list ? "list of " : "")$(typedict[type.type][1])"
        end...)
        """
    end

    # methodexpr = 
    quote
        """
            $($methodsym)
        
        $($description)

        ## Direct parameter

        $($(direct_param))
        """
        function $methodsym(obj, $(pos_args...); $(kw_args...))
            @objc [obj::id{SBApplication} $(method_args...)]::$returntype
        end
    end
end

function convert_parameters(params::Vector{Parameter}, typedict)
    kw_args_and_param_method_args = [convert_parameter(nothing, nothing, param, typedict) for param in params]
    kw_args = getindex.(kw_args_and_param_method_args, 1)
    param_method_args = getindex.(kw_args_and_param_method_args, 2)
    return kw_args, param_method_args
end

function convert_parameters(methodsym::Symbol, params::Vector{Parameter}, typedict)
    kw_args_and_param_method_args = [convert_parameter(i, methodsym, param, typedict) for (i, param) in enumerate(params)]
    kw_args = getindex.(kw_args_and_param_method_args, 1)
    param_method_args = getindex.(kw_args_and_param_method_args, 2)
    return kw_args, param_method_args
end

function convert_parameter(i::Union{Nothing, Int}, methodsym::Union{Symbol,Nothing}, param::Parameter, typedict)
    sym = Symbol(lowercasefirst(join(uppercasefirst.(split(param.name)))))
    # TODO correct type
    directtype, conversionhint = extract_typeinfo(param.types, typedict)
    # the first arg can be merged with the methodsym depending on the direct parameter
    slotsym = methodsym === nothing || i != 1 ? sym : Symbol(methodsym, join(uppercasefirst.(split(param.name))))
    method_arg = :($slotsym:$sym::$directtype)
    kwarg = param.optional ? Expr(:kw, sym, :(nil)) : sym
    return kwarg, method_arg
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

function make_typedict(enumerations, enumcodes, classes)
    # type, isvaluetype
    d = Dict{String,Tuple{Any,Bool}}(
        "text" => (NSString, false),
        "integer" => (NSInteger, true),
        "double integer" => (Int128, true), # not sure
        "boolean" => (Bool, true),
        "real" => (Cdouble, true),
        "rectangle" => (NSRect, false),
        "file" => (NSURL, false),
        "RGB color" => (NSColor, false),
        "point" => (NSPoint, false),
        "date" => (NSDate, false),
        "number" => (NSNumber, false),
        "list" => (SBElementArray, false),
        "type" => (NSObject, false), # not sure what's the right thing here
        "record" => (NSRecord, false), # not sure what's the right thing here
        "specifier" => (NSObject, false), # not sure what's the right thing here
        "property" => (NSObject, false), # not sure what's the right thing here
        "location specifier" => (NSObject, false), # not sure what's the right thing here
    )
    for (enum, enumcode) in zip(enumerations, enumcodes)
        enumsym = enumcode.args[3]::Symbol
        d[enum.name] = (:($enumsym.T), true)
    end
    for class in classes
        d[class.name] = transform_type_symbol(class.name), false
    end
    return d
end

function generate_code(d::Dictionary)
    enumcodes = []
    classcode_tuples = []

    suites = [s for s in d.suites if !s.hidden]

    enumerations = reduce(vcat, [s.enumerations for s in suites])
    enumcodes = map(generate_code, enumerations)
    
    classes = reduce(vcat, [s.classes for s in suites])
    classextensions = reduce(vcat, [s.classextensions for s in suites])

    merge_same_classes!(classes)
    merge_in_extensions!(classes, classextensions)

    typedict = make_typedict(enumerations, enumcodes, classes)

    class_set = Set([x.name for x in classes])
    commands = reduce(vcat, [s.commands for s in suites])
    commandcodes = map(c -> generate_code(c, class_set, typedict), commands)

    command_dict = Dict(c.name => c for c in commands)
    classcode_tuples = map(c -> generate_code(c, typedict, command_dict), classes)

    objcodes = getindex.(classcode_tuples, 1)
    propcodes = getindex.(classcode_tuples, 2)


    Expr(
        :block,
        Expr(:block, enumcodes...),
        Expr(:block, objcodes...),
        Expr(:block, propcodes...),
        Expr(:block, commandcodes...),
        # map(generate_code, s.commands)...,
    )
end

# the same class can be in the standard suite and the application suite
# for example, but we need one @objcproperties block per class
function merge_same_classes!(classes)
    d = Dict{String,Int}()
    to_remove = Int[]
    for (i, class) in enumerate(classes)
        if haskey(d, class.name)
            push!(to_remove, i)
            classes[d[class.name]] = merge_classes(classes[d[class.name]], class)
        else
            d[class.name] = i
        end
    end
    splice!(classes, to_remove)
    return
end

function merge_in_extensions!(classes, classextensions)
    extdict = Dict(map(c -> c.extends => c, classextensions))
    for i in eachindex(classes)
        c = classes[i]
        if haskey(extdict, c.name)
            classes[i] = merge_with_extension(c, extdict[c.name])
        end
    end
    return classes
end

function merge_with_extension(c::Class, ce::ClassExtension)
    Class(
        c.name,
        c.code,
        c.description,
        [c.respondsto; ce.respondsto],
        [c.properties; ce.properties],
        [c.elements; ce.elements],
        c.inherits,
    )
end

function merge_classes(class1::Class, class2::Class)
    if class1.inherits !== nothing && class2.inherits !== nothing && class1.inherits != class2.inherits
        error("inherits differs for $class1 and $class2")
    end
    Class(
        class1.name,
        class1.code,
        class1.description,
        [class1.respondsto; class2.respondsto],
        [class1.properties; class2.properties],
        [class1.elements; class2.elements],
        class1.inherits !== nothing ? class1.inherits : class2.inherits
    )
end

macro generate_module_from_sdef(namespace::Symbol, appname)

    dict = AppleScriptingBridge.sdef(appname);

    ex = AppleScriptingBridge.generate_code(dict)

    quote
        @eval module $namespace
            using EnumX
            using ObjectiveC
            using ObjectiveC.Foundation

            ObjectiveC.load_framework("ScriptingBridge")

            # that we need to import these is bad macro hygiene currently
            using AppleScriptingBridge: AppleScriptingBridge, SBElementArray, NSColor, NSPoint, SBObject

            $ex

            function application()
                bundleid = AppleScriptingBridge.bundle_identifier($appname)
                Application(@objc [SBApplication applicationWithBundleIdentifier:bundleid::id{NSString}]::id{Application})
            end
        end
    end
end

function bundle_identifier(appname::String)::String
    command = "id of application \"$appname\""
    readchomp(`osascript -e $command`)
end

function app_path(appname::String)::String
    command = "POSIX path of (path to application \"$appname\")"
    readchomp(`osascript -e $command`)
end

function app_sdef(appname)::String
    readchomp(`sdef $(app_path(appname))`)
end

function convert_result(i::id{T}, ::Nothing) where T
    T(i)
end

function convert_result(i::id{NSString}, ::Nothing)
    String(NSString(i))
end

function convert_result(x, ::Nothing)
    x
end

function convert_result(x::id{SBElementArray}, T::Type{<:SBElementArrayWrapper})
    T(SBElementArray(x))
end

end

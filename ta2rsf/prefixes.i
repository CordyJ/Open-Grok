% unit
% module Prefixes

    const * schemePrefix := "$_"
    const * attributePrefix := "@_"
    const * schemeAttributePrefix := schemePrefix + attributePrefix

    const * lenSchemePrefix := length (schemePrefix)
    const * lenAttributePrefix := length (attributePrefix)

    const * prefixRelEntity := "[?"
    const * suffixRelEntity := "?]"
    const * lenPrefixRelEntity := length (prefixRelEntity)
    const * lenSuffixRelEntity := length (suffixRelEntity)

    const * listPrefix := "$LIST_"
    const * strPrefix := "$STR_"

    const * lenListPrefix := length (listPrefix)
    const * lenStrPrefix := length (strPrefix)

    const * edgePrefix := "[_"
    const * edgeSuffix := "_]"
    const * edgeSeparator := ",_"

    const * lenEdgeSeparator := length (edgeSeparator)
    const * lenEdgePrefix := length (edgePrefix)
    const * lenEdgeSuffix := length (edgeSuffix)

    const * schemeListPrefix := schemePrefix + listPrefix
    const * schemeStrPrefix := schemePrefix + strPrefix
    const * lenSchemeListPrefix := length (schemeListPrefix)
    const * lenSchemeStrPrefix := length (schemeStrPrefix)

    const * noAttribute := "No_Attr"

    fcn edgeAsString (src, rel, trg : num) : num 
        const Src := numName (src)
        const Trg := numName (trg)
        const Rel := numName (rel)
%-         const edge := edgePrefix + Src + edgeSeparator + Rel + edgeSeparator + 
%-             Trg + edgeSuffix  %% Patch 000313 RCH
        const edge := edgePrefix + Rel + edgeSeparator + Src + edgeSeparator + 
            Trg + edgeSuffix
        result nameNum (edge)
    end edgeAsString

% end Prefixes


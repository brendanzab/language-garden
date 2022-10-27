type context

val initial_context : context

val elab_items : context -> SurfaceSyntax.item list -> CoreSyntax.item list

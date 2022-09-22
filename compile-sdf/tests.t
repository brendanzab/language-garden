Generate the GLSL for the scene
  $ compile-sdf
  void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    // Normalise the UV coordinates to <-0.5,0.5>
    vec2 uv = fragCoord / iResolution.xy - 0.5;
    // Fix the aspect ratio of the x axis
    uv.x *= iResolution.x / iResolution.y;
  
    // Compute the colour for this UV coordinate.
    vec3 col = mix(vec3(0., 0., 0.), vec3(1., 1., 1.), step(0.0, min((length(vec2(abs(uv.x), uv.y) - vec2(0., 0.)) - 0.3), (max(abs(uv.x - vec2(0.2, 0.).x), abs(uv.y - vec2(0.2, 0.).y)) - 0.2))));
    // Output to screen
    fragColor = vec4(col,1.0);
  }

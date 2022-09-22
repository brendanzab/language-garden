Generate the GLSL for the scene
  $ compile-sdf
  void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    // Normalise the UV coordinates to <-0.5,0.5>
    vec2 uv = fragCoord / iResolution.xy - 0.5;
    // Fix the aspect ratio of the x axis
    uv.x *= iResolution.x / iResolution.y;
  
    // Compute the colour for this UV coordinate.
    vec3 col = mix(vec3(1., 1., 1.), vec3(0.35, 0.45, 0.5), step(0.0, min((length((vec2(abs(uv.x), uv.y) - vec2(0.2, 0.))) - 0.1), (max(abs((uv - vec2(0.1, 0.2)).x), abs((uv - vec2(0.1, 0.2)).y)) - 0.15))));
    // Output to screen
    fragColor = vec4(col,1.0);
  }

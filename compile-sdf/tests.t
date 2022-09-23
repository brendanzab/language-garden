Generate the GLSL for the scene
  $ compile-sdf
  // The main entrypoint of the shader.
  //
  // Copy and paste this into https://www.shadertoy.com/new to see the output.
  void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    // Normalise the UV coordinates to <-0.5,0.5>
    vec2 uv = fragCoord / iResolution.xy - 0.5;
    // Fix the aspect ratio of the x axis
    uv.x *= iResolution.x / iResolution.y;
  
    // Local bindings
    vec2 t0 = vec2(0.2, 0.2);
    vec2 t1 = uv - vec2(0.1, 0.2);
  
    // Compute the colour for this UV coordinate.
    vec3 color = mix(vec3(1., 1., 1.), vec3(0.35, 0.45, 0.5), step(0.0, min(length(mod((uv) + 0.5 * t0, t0) - 0.5 * t0) - (0.05), max(abs(t1.x), abs(t1.y)) - (0.15))));
  
    // Output to screen
    fragColor = vec4(color,1.0);
  }

Generate the GLSL for the scene
  $ compile-sdf
  // The main entrypoint of the shader.
  //
  // Copy and paste this into https://www.shadertoy.com/new to see the output.
  void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    // Normalise the UV coordinates to (-0.5, 0.5)
    vec2 uv = fragCoord / iResolution.xy - 0.5;
    // Fix the aspect ratio of the x axis
    uv.x *= iResolution.x / iResolution.y;
  
    // Local bindings
    vec3 t0 = vec3(1., 1., 1.);
    vec3 t1 = vec3(0.35, 0.45, 0.5);
    vec3 t2 = vec3(0.85, 0.85, 0.7);
    vec2 t3 = uv + 0.5;
    float t4 = t3.y;
    float t5 = t3.x;
    float t6 = t5 * 0.2;
    float t7 = t4 + t6;
    vec3 t8 = mix(t1, t2, t7);
    vec2 t9 = vec2(0.2, 0.2);
    vec2 t10 = t9 * 0.5;
    vec2 t11 = uv + t10;
    vec2 t12 = mod(t11, t9);
    vec2 t13 = t12 - t10;
    float t14 = length(t13);
    float t15 = t14 - 0.05;
    vec2 t16 = abs(uv);
    vec2 t17 = t16 - t9;
    float t18 = t17.x;
    float t19 = abs(t18);
    float t20 = t17.y;
    float t21 = abs(t20);
    float t22 = max(t19, t21);
    float t23 = t22 - 0.15;
    float t24 = min(t15, t23);
    float t25 = step(0., t24);
    vec3 t26 = mix(t0, t8, t25);
  
    // Compute the colour for this UV coordinate.
    vec3 color = t26;
  
    // Output to screen
    fragColor = vec4(color,1.0);
  }

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
    vec3 t0 = vec3(1., 1., 1.);
    vec3 t1 = vec3(0.35, 0.45, 0.5);
    vec2 t2 = vec2(0.2, 0.2);
    vec2 t3 = t2 * 0.5;
    vec2 t4 = uv + t3;
    vec2 t5 = vec2(0.2, 0.2);
    vec2 t6 = mod(t4, t5);
    vec2 t7 = vec2(0.2, 0.2);
    vec2 t8 = t7 * 0.5;
    vec2 t9 = t6 - t8;
    float t10 = length(t9);
    float t11 = t10 - 0.05;
    vec2 t12 = abs(uv);
    vec2 t13 = vec2(0.2, 0.2);
    vec2 t14 = t12 - t13;
    float t15 = t14.x;
    float t16 = abs(t15);
    vec2 t17 = abs(uv);
    vec2 t18 = vec2(0.2, 0.2);
    vec2 t19 = t17 - t18;
    float t20 = t19.y;
    float t21 = abs(t20);
    float t22 = max(t16, t21);
    float t23 = t22 - 0.15;
    float t24 = min(t11, t23);
    float t25 = step(0., t24);
    vec3 t26 = mix(t0, t1, t25);
  
    // Compute the colour for this UV coordinate.
    vec3 color = t26;
  
    // Output to screen
    fragColor = vec4(color,1.0);
  }

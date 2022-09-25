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
    vec3 t2 = vec3(0.85, 0.85, 0.7);
    vec2 t3 = uv + 0.5;
    float t4 = t3.y;
    vec2 t5 = uv + 0.5;
    float t6 = t5.x;
    float t7 = t6 * 0.2;
    float t8 = t4 + t7;
    vec3 t9 = mix(t1, t2, t8);
    vec2 t10 = vec2(0.2, 0.2);
    vec2 t11 = t10 * 0.5;
    vec2 t12 = uv + t11;
    vec2 t13 = vec2(0.2, 0.2);
    vec2 t14 = mod(t12, t13);
    vec2 t15 = vec2(0.2, 0.2);
    vec2 t16 = t15 * 0.5;
    vec2 t17 = t14 - t16;
    float t18 = length(t17);
    float t19 = t18 - 0.05;
    vec2 t20 = abs(uv);
    vec2 t21 = vec2(0.2, 0.2);
    vec2 t22 = t20 - t21;
    float t23 = t22.x;
    float t24 = abs(t23);
    vec2 t25 = abs(uv);
    vec2 t26 = vec2(0.2, 0.2);
    vec2 t27 = t25 - t26;
    float t28 = t27.y;
    float t29 = abs(t28);
    float t30 = max(t24, t29);
    float t31 = t30 - 0.15;
    float t32 = min(t19, t31);
    float t33 = step(0., t32);
    vec3 t34 = mix(t0, t9, t33);
  
    // Compute the colour for this UV coordinate.
    vec3 color = t34;
  
    // Output to screen
    fragColor = vec4(color,1.0);
  }

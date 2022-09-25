Generate the GLSL for the scene
  $ compile-sdf
  // The main entrypoint of the shader.
  //
  // Copy and paste this into https://www.shadertoy.com/new to see the output.
  void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    // Local bindings
    vec3 t0 = vec3(1., 1., 1.);
    vec3 t1 = vec3(0.35, 0.45, 0.5);
    vec3 t2 = vec3(0.85, 0.85, 0.7);
    vec2 t3 = iResolution.xy;
    vec2 t4 = fragCoord / t3;
    vec2 t5 = t4 - 0.5;
    float t6 = t5.x;
    float t7 = iResolution.x;
    float t8 = iResolution.y;
    float t9 = t7 / t8;
    float t10 = t6 * t9;
    float t11 = t5.y;
    vec2 t12 = vec2(t10, t11);
    vec2 t13 = t12 + 0.5;
    float t14 = t13.y;
    float t15 = t13.x;
    float t16 = t15 * 0.2;
    float t17 = t14 + t16;
    vec3 t18 = mix(t1, t2, t17);
    vec2 t19 = vec2(0., 0.);
    vec2 t20 = t12 - t19;
    float t21 = length(t20);
    float t22 = t21 - 0.3;
    vec2 t23 = vec2(0.2, 0.);
    vec2 t24 = t12 - t23;
    float t25 = t24.x;
    float t26 = abs(t25);
    float t27 = t24.y;
    float t28 = abs(t27);
    float t29 = max(t26, t28);
    float t30 = t29 - 0.2;
    float t31 = min(t22, t30);
    float t32 = max(0.05, t31);
    float t33 = 0.05 - t22;
    float t34 = 0.05 - t30;
    vec2 t35 = vec2(t33, t34);
    vec2 t36 = max(t35, t19);
    float t37 = length(t36);
    float t38 = t32 - t37;
    float t39 = step(0., t38);
    vec3 t40 = mix(t0, t18, t39);
  
    // Compute the colour for this UV coordinate.
    vec3 color = t40;
  
    // Output to screen
    fragColor = vec4(color, 1.0);
  }

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
    vec2 t19 = vec2(0.2, 0.2);
    vec2 t20 = t19 * 0.5;
    vec2 t21 = t12 + t20;
    vec2 t22 = mod(t21, t19);
    vec2 t23 = t22 - t20;
    float t24 = length(t23);
    float t25 = t24 - 0.05;
    vec2 t26 = abs(t12);
    vec2 t27 = t26 - t19;
    float t28 = t27.x;
    float t29 = abs(t28);
    float t30 = t27.y;
    float t31 = abs(t30);
    float t32 = max(t29, t31);
    float t33 = t32 - 0.15;
    float t34 = min(t25, t33);
    float t35 = step(0., t34);
    vec3 t36 = mix(t0, t18, t35);
  
    // Compute the colour for this UV coordinate.
    vec3 color = t36;
  
    // Output to screen
    fragColor = vec4(color, 1.0);
  }

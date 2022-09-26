Generate the GLSL for the scene
  $ compile-sdf
  // The main entrypoint of the shader.
  //
  // Copy and paste this into https://www.shadertoy.com/new to see the output.
  void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    // Local bindings
    vec3 t0 = vec3(0.25, 0.25, 0.25);
    vec3 t1 = vec3(1., 1., 1.);
    vec3 t2 = vec3(0.35, 0.45, 0.5);
    vec3 t3 = vec3(0.85, 0.85, 0.7);
    vec2 t4 = iResolution.xy;
    vec2 t5 = fragCoord / t4;
    vec2 t6 = t5 - 0.5;
    float t7 = t6.x;
    float t8 = iResolution.x;
    float t9 = iResolution.y;
    float t10 = t8 / t9;
    float t11 = t7 * t10;
    float t12 = t6.y;
    vec2 t13 = vec2(t11, t12);
    vec2 t14 = t13 + 0.5;
    float t15 = t14.y;
    float t16 = t14.x;
    float t17 = t16 * 0.2;
    float t18 = t15 + t17;
    vec3 t19 = mix(t2, t3, t18);
    vec2 t20 = vec2(0., 0.);
    vec2 t21 = t13 - t20;
    float t22 = length(t21);
    float t23 = t22 - 0.3;
    vec2 t24 = vec2(0.2, 0.);
    vec2 t25 = t13 - t24;
    vec2 t26 = abs(t25);
    float t27 = t26.x;
    float t28 = t26.y;
    float t29 = max(t27, t28);
    float t30 = t29 - 0.2;
    float t31 = min(t23, t30);
    float t32 = max(0.05, t31);
    float t33 = 0.05 - t23;
    float t34 = 0.05 - t30;
    vec2 t35 = vec2(t33, t34);
    vec2 t36 = max(t35, t20);
    float t37 = length(t36);
    float t38 = t32 - t37;
    float t39 = step(0., t38);
    vec3 t40 = mix(t1, t19, t39);
    vec2 t41 = vec2(-0.3, -0.2);
    vec2 t42 = t13 - t41;
    vec2 t43 = abs(t42);
    vec2 t44 = vec2(0.3, 0.2);
    vec2 t45 = t43 - t44;
    vec2 t46 = max(t45, t20);
    float t47 = length(t46);
    float t48 = t45.x;
    float t49 = t45.y;
    float t50 = max(t48, t49);
    float t51 = min(t50, 0.);
    float t52 = t47 + t51;
    float t53 = step(0., t52);
    vec3 t54 = mix(t0, t40, t53);
  
    // Compute the colour for this UV coordinate.
    vec3 color = t54;
  
    // Output to screen
    fragColor = vec4(color, 1.0);
  }

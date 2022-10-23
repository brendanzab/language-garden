// The main entrypoint of the shader.
//
// Copy and paste this into https://www.shadertoy.com/new to see the output.
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
  vec3 t0 = vec3(1., 1., 1.);
  vec3 t1 = vec3(0.35, 0.45, 0.5);
  vec2 t2 = iResolution.xy;
  vec2 t3 = fragCoord / t2;
  vec2 t4 = t3 - 0.5;
  float t5 = t4.x;
  float t6 = t2.x;
  float t7 = t2.y;
  float t8 = t6 / t7;
  float t9 = t5 * t8;
  float t10 = t4.y;
  vec2 t11 = vec2(t9, t10);
  vec2 t12 = vec2(0., 0.);
  vec2 t13 = t11 - t12;
  float t14 = length(t13);
  float t15 = t14 - 0.3;
  vec2 t16 = vec2(0.2, 0.);
  vec2 t17 = t11 - t16;
  vec2 t18 = abs(t17);
  float t19 = t18.x;
  float t20 = t18.y;
  float t21 = max(t19, t20);
  float t22 = t21 - 0.2;
  float t23 = min(t15, t22);
  float t24 = max(0.05, t23);
  float t25 = 0.05 - t15;
  float t26 = 0.05 - t22;
  vec2 t27 = vec2(t25, t26);
  vec2 t28 = max(t27, t12);
  float t29 = length(t28);
  float t30 = t24 - t29;
  float t31 = step(0., t30);
  vec3 t32 = mix(t0, t1, t31);

  // Set the color of the current pixel
  fragColor = vec4(t32, 1.0);
}

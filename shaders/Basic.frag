#version 460

in vec3 vecPosition;

out vec4 outColor;

void main() {
  float height = vecPosition.y * 2 + 0.3;


  float red = mix(0.3, 0.5, height);
  float green = mix(0.6, 0.2, height);

  outColor = vec4(red, green, 0, 1);
}

#version 460

in vec3 vecPosition;

out vec4 outColor;

void main() {
  float height = vecPosition.y * 2 + 0.3;

  outColor = vec4(height, 1 - height, 1 - height / 2, 1.0);
}

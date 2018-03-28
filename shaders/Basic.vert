in vec3 position;

layout (location = 0) uniform mat4 projection;
layout (location = 1) uniform mat4 view;
layout (location = 2) uniform mat4 model;

out vec3 vecPosition;

void main() {
  vecPosition = position;
  gl_Position = projection * view * model * vec4(position, 1.0);
}

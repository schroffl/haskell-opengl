in vec3 position;

uniform mat4 projection;
uniform mat4 view;
uniform mat4 model;

out vec3 vecPosition;

void main() {
  vecPosition = position;
  gl_Position = projection * view * model * vec4(position, 1.0);
}

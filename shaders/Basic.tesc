#version 460

layout(vertices = 4) out;

layout (location = 3) uniform float time;
layout (location = 4) uniform vec3 cameraPos;

in vec3 vPosition[];

out vec3 tcPosition[];

int getOuterLevel(float d) {
  if(d < 10)
    return 5;
  else if(d < 15)
    return 4;
  else if(d < 20)
    return 3;
  else if(d < 30)
    return 2;
  else
    return 1;
}

int getInnerLevel(float d) {
  if(d < 10)
    return 4;
  else if(d < 15)
    return 3;
  else
    return 1;
}

void main(void) {
  vec3 vertPosition = gl_in[gl_InvocationID].gl_Position.xyz;
  float d = distance(cameraPos, vertPosition);

  int outer = getOuterLevel(d);
  int inner = getInnerLevel(d);

  gl_TessLevelOuter[0] = outer;
  gl_TessLevelOuter[1] = outer;
  gl_TessLevelOuter[2] = outer;
  gl_TessLevelOuter[3] = outer;

  gl_TessLevelInner[0] = inner;
  gl_TessLevelInner[1] = inner;

  tcPosition[gl_InvocationID] = vPosition[gl_InvocationID];

  gl_out[gl_InvocationID].gl_Position = gl_in[gl_InvocationID].gl_Position;
}

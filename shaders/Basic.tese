#version 460

layout(quads, equal_spacing, cw) in;

in vec3 tcPosition[];

out vec3 tePosition;

//quad interpol
vec4 interpolate(in vec4 v0, in vec4 v1, in vec4 v2, in vec4 v3) {
  vec4 a = mix(v0, v1, gl_TessCoord.x);
  vec4 b = mix(v3, v2, gl_TessCoord.x);

  return mix(a, b, gl_TessCoord.y);
}

vec3 lerpVertexPos(in vec3 v1, in vec3 v2, in vec3 v3, in vec3 v4) {
  vec3 a = mix(v1, v2, gl_TessCoord.x);
  vec3 b = mix(v4, v3, gl_TessCoord.x);

  return mix(a, b, gl_TessCoord.y);
}

void main() { 
  // tePosition = tcPosition[0];

  tePosition = lerpVertexPos(
      tcPosition[0],
      tcPosition[1],
      tcPosition[2],
      tcPosition[3]
    );


  gl_Position = interpolate(
      gl_in[0].gl_Position, 
      gl_in[1].gl_Position, 
      gl_in[2].gl_Position, 
      gl_in[3].gl_Position
    );
}

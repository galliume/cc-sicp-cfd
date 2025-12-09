module Mesh;

import Defines;

namespace cfd
{
  Coordinate Cell::x() const
  {
    return _mesh->x(_index);
  }
}

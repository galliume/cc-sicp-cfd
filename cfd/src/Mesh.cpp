module;
#include <cassert>

module Mesh;

import Defines;

namespace cfd
{
  Cell::Cell(Mesh const & mesh, Index index)
  : _mesh(&mesh)
  , _index(index)
  {
    assert(_mesh != nullptr);
    assert(_index >= 0);
    assert(_index < _mesh->n_cells());
  }

  Coordinate Cell::x() const
  {
    return _mesh->x(_index);
  }
}

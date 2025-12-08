module;

#include <cassert>
#include <cstddef>
#include <vector>

export module Mesh;

export namespace cfd
{
  using Coordinate = double;
  using Index = std::size_t;
  using Meters = double;
  using Scalar = double;

  class Mesh
  {
    public:
      Mesh(Index n, Meters m)
      : _n_cells(n)
      , _length(m)
      {
        assert(_n_cells > 0);

        _dx = _length / static_cast<Meters>(_n_cells);

        _x_centers.resize(_n_cells);
        for (Index i { 0 }; i < _n_cells; ++i) {
          _x_centers[i] = _dx / 2.0 + static_cast<Meters>(i) * _dx;
        }
      }

      Index n_cells() const { return _n_cells; }
      Meters length() const { return _length; }
      Meters dx() const { return _dx; }
      std::vector<Coordinate> const & x_centers() const { return _x_centers; }
      Coordinate x(Index i) const { return _x_centers[i]; }

    private:
      Index _n_cells;
      Meters _length;
      Meters _dx;
      std::vector<Coordinate> _x_centers;
  };
}

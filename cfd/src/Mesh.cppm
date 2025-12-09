module;

#include <cassert>
#include <cstddef>
//#include <generator>
#include <iterator>
#include <vector>

import Defines;

export module Mesh;

export namespace cfd
{
  class Mesh;

  struct Cell
  {
    public:
      Cell(Mesh const & mesh, Index index);

      [[nodiscard]] Index index() const { return _index; }
      Coordinate x() const;

    private:
      Mesh const * _mesh;
      Index _index;
  };

  class Mesh
  {
    public:
      class iterator
      {
        public:
          using iterator_concept  = std::forward_iterator_tag;
          using iterator_category = std::forward_iterator_tag;
          using value_type        = Cell;
          using difference_type   = std::ptrdiff_t;

          value_type operator*() const { return Cell(*_mesh, _index); }

          iterator & operator++()
          {
            ++_index;
            return *this;
          }

          iterator operator++(int)
          {
            iterator tmp = *this;
            ++(*this);
            return tmp;
          }

          bool operator==(iterator const & other) const { return _index == other._index; }

        private:
          friend class Mesh;
          iterator(Mesh const * mesh, Index index) : _mesh(mesh), _index(index) {}
          Mesh const * _mesh;
          Index _index;
      };

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

      // std::generator<Cell> cells() const {
      //   for (Index i { 0 }; i < _n_cells; ++i) {
      //     co_yield Cell(*this, i);
      //   }
      // }
      iterator begin() const { return iterator(this, 0); }
      iterator end() const { return iterator(this, _n_cells); }

    private:
      Index _n_cells;
      Meters _length;
      Meters _dx;
      std::vector<Coordinate> _x_centers;
  };
}

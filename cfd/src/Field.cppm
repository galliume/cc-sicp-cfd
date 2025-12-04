module;

#include <cstddef>
#include <iostream>
#include <format>
#include <fstream>
#include <string>
#include <vector>

export module Field;

import Mesh;

export namespace cfd
{
  class Field
  {
    public:
      Field(std::string_view name,
        Mesh const & mesh,
        Scalar const initial_value)
      : _name(name)
      , _mesh(mesh)
      , _values(mesh.n_cells(), initial_value)
      {

      }

      Scalar & operator[](Index i) { return _values[i]; }
      Scalar const & operator[](Index i) const { return _values[i]; }
      Index size() const { return _values.size(); }
      std::string_view name() const { return _name; }

      void to_string() const
      {
        std::cout << std::format("Field {} of size {} \n", _name, _values.size());
      }

      void to_csv(std::string const & filename) const
      {
        std::ofstream file(filename);
        file << "x,value\n";
        for(Index i { 0 }; i < size(); i++) {
            file << _mesh.x(i) << "," << _values[i] << "\n";
        }
        file.close();
        std::cout << std::format("Data written to {} \n", filename);
      }

    private:
      std::string _name;
      Mesh const & _mesh;
      std::vector<Scalar> _values;

  };
}

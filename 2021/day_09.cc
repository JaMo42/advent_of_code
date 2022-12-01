#include <cstdlib>
#include <cstdio>
#include <cstdint>
#include <cstring>
#include <vector>
#include <utility>
#include <algorithm>

class Heightmap
{
public:
  int8_t at (std::size_t x, std::size_t y) const { return M_data[y][x]; }

  int8_t at_or (std::size_t x, std::size_t y, int8_t or_ = 9) const
  {
    // If values would be less than zero they overflow to being greater than
    // the width or height
    if (x >= M_width || y >= M_height)
      return or_;
    return M_data[y][x];
  }

  std::size_t width () const { return M_width; }

  std::size_t height () const { return M_height; }

  static Heightmap from_file (std::FILE *fp)
  {
    static char read_buf[512];
    Heightmap m {};
    std::size_t rows = 0, cols;
    std::vector<int8_t> line;

    while (std::fgets (read_buf, std::size (read_buf), fp))
      {
        line.clear ();
        char *p = read_buf;
        while (*p && *p != '\n')
          {
            line.emplace_back (*p - '0');
            ++p;
          }
        m.M_data.push_back (line);
        ++rows;
        cols = p - read_buf;
      }
    m.M_width = cols;
    m.M_height = rows;
    return m;
  }

private:
  std::vector<std::vector<int8_t>> M_data;
  std::size_t M_width, M_height;
};


bool
is_low_point (std::size_t x, std::size_t y, const Heightmap &m)
{
  const int8_t v = m.at (x, y);
  return (m.at_or (x-1, y) > v
          && m.at_or (x+1, y) > v
          && m.at_or (x, y-1) > v
          && m.at_or (x, y+1) > v);

}


unsigned
low_point_risk (const Heightmap &m)
{
  unsigned risk = 0;
  for (std::size_t y = 0; y < m.height (); ++y)
    {
      for (std::size_t x = 0; x < m.width (); ++x)
        {
          if (is_low_point (x, y, m))
            risk += m.at (x, y) + 1;
        }
    }
  return risk;
}


void
basin_size_impl (std::size_t x, std::size_t y, const Heightmap &m,
                 std::vector<std::vector<bool>> &seen, unsigned &v)
{
  // Stop flood fill if position is out of bounds or already seen or a 9
  if (x == static_cast<std::size_t> (-1) || x == m.width ()
      || y == static_cast<std::size_t> (-1) || y == m.height ())
    return;
  if (seen[y][x] || m.at (x, y) == 9)
    return;

  // Increment counter and mark cell as seen
  ++v;
  seen[y][x] = true;

  // Recurse for each neightboor
  basin_size_impl (x+1, y, m, seen, v);
  basin_size_impl (x-1, y, m, seen, v);
  basin_size_impl (x, y+1, m, seen, v);
  basin_size_impl (x, y-1, m, seen, v);
}


unsigned
basin_size (std::size_t x, std::size_t y, const Heightmap &m)
{
  // Note: since this is static, it will not be reset between calls and old
  // marked cells will stay but since all basins are separated this does not
  // matter
  static std::vector<std::vector<bool>> seen (m.height (),
                                              std::vector<bool> (m.width ()));
  unsigned size = 0;
  basin_size_impl (x, y, m, seen, size);
  return size;
}


unsigned
largest_basins_size (const Heightmap &m, unsigned basin_count = 3)
{
  std::vector<unsigned> sizes {};
  unsigned total = 0;

  // Get basin sizes
  for (std::size_t y = 0; y < m.height (); ++y)
    {
      for (std::size_t x = 0; x < m.width (); ++x)
        {
          if (is_low_point (x, y, m))
            sizes.push_back (basin_size (x, y, m));
        }
    }

  if (sizes.empty ())
    return 0;

  // Sort by size (low -> high)
  std::sort (sizes.begin (), sizes.end ());

  // Clamp count to calculate total for
  if (basin_count > sizes.size ())
    basin_count = sizes.size ();
  else if (basin_count < 1)
    basin_count = 1;

  // Multiply sizes, starting with the largest possible
  total = sizes[sizes.size () - basin_count];
  for (std::size_t i = sizes.size () - basin_count + 1; i < sizes.size (); ++i)
    total *= sizes[i];

  return total;
}


int
main ()
{
  const Heightmap m = Heightmap::from_file (stdin);

  const unsigned risk = low_point_risk (m);
  std::printf ("Sum of low point risk levels:  \x1b[92m%u\x1b[0m\n", risk);

  const unsigned basin_size = largest_basins_size (m);
  std::printf ("Size of three largest bassins: \x1b[92m%u\x1b[0m\n", basin_size);
}


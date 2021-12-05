#include <cstdio>
#include <bitset>
#include <cstring>
#include <climits>
#include <vector>
#include <algorithm>

// Maximum amount of bits supported
constexpr std::size_t Bits = sizeof (void *) * CHAR_BIT;

using bitset_type = std::bitset<Bits>;

bool
get_common (bool most_common, size_t pos,
            const std::vector<bitset_type> &values)
{
  unsigned set = 0, unset = 0;
  for (const auto &v : values)
    {
      if (v.test (pos))
        ++set;
      else
        ++unset;
    }
  return most_common ? set >= unset : set < unset;
}

void
part_one (const std::vector<bitset_type> &values, std::size_t bits)
{
  bitset_type gamma_rate {0}, epsilon_rate {0};

  for (std::size_t i = 0; i < bits; ++i)
    {
      gamma_rate.set (i, get_common (true, i, values));
      epsilon_rate.set (i, get_common (false, i, values));
    }

  std::printf ("Gamma            :   %s (%llu)\n",
               gamma_rate.to_string ().c_str () + Bits - bits,
               gamma_rate.to_ullong ());
  std::printf ("Epsilon          : %s (%llu)\n",
               epsilon_rate.to_string ().c_str () + Bits - bits,
               epsilon_rate.to_ullong ());
  std::printf ("Power consumption: %llu\n",
               gamma_rate.to_ullong () * epsilon_rate.to_ullong ());
}

bitset_type
filter (std::vector<bitset_type> &values, std::size_t pos, bool most_common)
{
  bool common = get_common (most_common, pos, values);
  auto it = std::remove_if (values.begin (), values.end (),
                            [&](bitset_type &v) -> bool {
                              return v.test (pos) != common;
                            });
  values.erase (it, values.end ());
  if (values.size () == 1)
    return values.front ();
  return filter (values, pos - 1, most_common);
}

void
part_two (const std::vector<bitset_type> &values, std::size_t bits)
{
  auto oxygen_values = values, co2_values = values;

  unsigned long long oxygen = filter (oxygen_values, bits - 1, true).to_ullong ();
  unsigned long long co2 = filter (co2_values, bits - 1, false).to_ullong ();

  std::printf ("Oxygen generator: %llu\n", oxygen);
  std::printf ("CO2 scrubber    : %llu\n", co2);
  std::printf ("Lifetime support: %llu\n", oxygen * co2);
}

int main ()
{
  char buf[Bits + 1];
  char *end;
  std::vector<bitset_type> values;

  while (std::fgets (buf, sizeof (buf), stdin))
    {
      unsigned long n = std::strtoul (buf, &end, 2);
      values.emplace_back (n);
    }

  std::size_t bits = std::strlen (buf);
  if (buf[bits - 1] == '\n')
    --bits;

  std::puts ("==== PART ONE ====");
  part_one (values, bits);
  std::puts ("==== PART TWO ====");
  part_two (values, bits);
}


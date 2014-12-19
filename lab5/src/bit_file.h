#ifndef BIT_FILE_H_c3dca5908aed43f251ac818e4cf02817
#	define BIT_FILE_H_c3dca5908aed43f251ac818e4cf02817

#include <string>

class bit_file
{
public:
	bit_file (const std::string& path, bool trunc = false);
	~bit_file ();

	inline size_t size () const { return size_; };
	void push (bool bit);
	bool pop  ();
	void save () const;

private:
	std::string path_;
	size_t size_;
	size_t seek_;
	std::string data_;
};

#endif

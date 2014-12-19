#include "bit_file.h"

#include <stdexcept>
#include <fstream>
#include <iostream>

bit_file::bit_file (const std::string& path, bool trunc)
	: path_(path), size_(0), seek_(0)
{
	if (trunc) {
		std::ofstream file(path_.c_str(), std::ios::binary | std::ios::out | std::ios::trunc);
		if (file.fail())
			throw std::runtime_error("could not open for writing");
		file.close();
	}
	else {
		std::ifstream file(path_.c_str(), std::ios::binary | std::ios::in);
		if (file.fail())
			throw std::runtime_error("could not open for reading");

		char buf[1024];
		size_t n = 1;
		while (n) {
			n = file.readsome(buf, sizeof(buf));
			data_.append(buf, n);
		}
		file.close();

		if (data_.length() < 4)
			throw std::runtime_error("not a valid bit_file");
		size_ = *((int* )data_.c_str());
		data_ = data_.substr(sizeof(size_));
	}
}

bit_file::~bit_file ()
{
}

void bit_file::push (bool bit)
{
	//std::cout << (bit ? '1' : '0');

	size_t pos = size_ % 8;

	if (!pos)
		data_ += '\0';

	if (bit)
		data_[size_ / 8] |= (1 << (7 - pos));
	else
		data_[size_ / 8] &= ~(1 << (7 - pos));

	size_++;
}

bool bit_file::pop ()
{
	if (seek_ >= size_)
		throw std::out_of_range("bit_file::pop");

	bool r = data_[seek_ / 8] & (1 << (7 - seek_ % 8));
	//std::cout << (r ? '1' : '0');
	seek_++;
	return r;
}

void bit_file::save () const
{
	std::ofstream file(path_.c_str(), std::ios::binary | std::ios::out | std::ios::trunc);
	if (file.fail())
		throw std::runtime_error("could not open for writing");
	file.write((char* )&size_, sizeof(size_));
	file.write(data_.c_str(), data_.length());
	file.close();
}

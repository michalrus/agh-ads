#include <iostream>
#include <stdexcept>
#include <map>
#include <set>
#include <fstream>
#include <boost/shared_ptr.hpp>

#include "bit_file.h"

/**
 * @brief Huffman's algorithm implementation (decompression).
 */

void huffman_d (const std::string& path);

/**
 * @brief Huffman's algorithm implementation (compression).
 */

void huffman_c (const std::string& path);

/**
 * @brief Entry point.
 */

int main (int ac, char** av)
{
	bool compress = false;

	try {
		if (ac == 3) {
			compress = (std::string(av[1]) == "-c");
			if (!compress && (std::string(av[1]) != "-x"))
				throw std::runtime_error("usage");
		}
		else
			throw std::runtime_error("usage");
	}
	catch (std::runtime_error& e) {
		std::cerr << "Usage:"
			<< "\t" << av[0] << " -c <file>" << std::endl
			<< "\t" << av[0] << " -x <file>" << std::endl;
		return -1;
	}

	try {
		if (compress)
			huffman_c(av[2]);
		else
			huffman_d(av[2]);
	}
	catch (std::runtime_error& e) {
		std::cerr << av[2] << ": " << e.what() << std::endl;
		return -2;
	}

	return 0;
}

struct tree {
	tree (char _b, size_t _n) /* leaf */
		: b(_b), n(_n)
	{
	}

	tree (boost::shared_ptr<tree> _l, boost::shared_ptr<tree> _r)
		: b('\0'), n(_l->n + _r->n), l(_l), r(_r)
	{
	}

	tree (boost::shared_ptr<tree> _l)	/* root that supports one(-unique)-byte file (= */
		: b('\0'), n(_l->n), l(_l)
	{
	}

	char b;
	size_t n;
	boost::shared_ptr<tree> l, r;
};

bool operator< (const boost::shared_ptr<tree>& a, const boost::shared_ptr<tree>& b)
{
	return a->n < b->n;
}

void huffman_c_preorder (boost::shared_ptr<tree> node, std::map<char, std::string>& map, std::string path = "")
{
	if (!node->l && !node->r) {
		map[node->b] = path;
		return;
	}

	if (node->l)
		huffman_c_preorder(node->l, map, path + '0');
	if (node->r)
		huffman_c_preorder(node->r, map, path + '1');
}

void huffman_c_save_tree (bit_file& file, boost::shared_ptr<tree> node)
{
	if (!node) {
		file.push(0);
		return;
	}

	file.push(1);
	if (!node->l && !node->r) { /* leaf */
		file.push(1);
		for (size_t i = 0; i < 8; i++)
			file.push(node->b & (1 << i));
	}
	else
		file.push(0);

	huffman_c_save_tree(file, node->l);
	huffman_c_save_tree(file, node->r);
}

void huffman_c (const std::string& path)
{
	std::string data;

	{
		std::ifstream file(path.c_str(), std::ios::binary | std::ios::in);
		if (file.fail())
			throw std::runtime_error("could not open for reading");

		char buf[1024];
		size_t n = 1;
		while (n) {
			n = file.readsome(buf, sizeof(buf));
			data.append(buf, n);
		}
		file.close();
	}

	if (data.empty())
		return;

	boost::shared_ptr<tree> t;

	/* build tree */
	{
		std::map<char, size_t> cmap;
		size_t i;

		for (i = 0; i < data.length(); i++)
			if (cmap.find(data[i]) == cmap.end())
				cmap[data[i]] = 1;
			else
				cmap[data[i]]++;

		std::multiset<boost::shared_ptr<tree> > tset;
		std::map<char, size_t>::iterator itr;
		for (itr = cmap.begin(); itr != cmap.end(); itr++) {
			boost::shared_ptr<tree> tmp(new tree(itr->first, itr->second));
			tset.insert(tmp);
		}

		std::multiset<boost::shared_ptr<tree> >::iterator st, nd;
		while (tset.size() > 2) {
			st = nd = tset.begin(); nd++;
			boost::shared_ptr<tree> tmp(new tree(*st, *nd));
			tset.erase(nd);
			tset.erase(st);
			tset.insert(tmp);
		}

		st = nd = tset.begin();
		if (tset.size() == 2) {
			nd++;
			boost::shared_ptr<tree> tmp(new tree(*st, *nd));
			t = tmp;
		}
		else {
			boost::shared_ptr<tree> tmp(new tree(*st));
			t = tmp;
		}
	}

	std::map<char, std::string> hmap;
	huffman_c_preorder(t, hmap);

	{
		bit_file file(path, true);

		huffman_c_save_tree(file, t);

		size_t i, j;
		for (i = 0; i < data.length(); i++) {
			std::string path(hmap[data[i]]);
			for (j = 0; j < path.length(); j++)
				file.push(path[j] == '1');
		}
		file.save();
	}
}

void huffman_d_read_tree (bit_file& file, boost::shared_ptr<tree>& node)
{
	if (!file.pop()) /* empty branch */
		return;

	if (file.pop()) { /* leaf */
		char b = '\0';
		for (size_t i = 0; i < 8; i++)
			if (file.pop())
				b |= 1 << i;
		boost::shared_ptr<tree> tmp(new tree(b, 0));
		node = tmp;
	}
	else { /* branch */
		boost::shared_ptr<tree> tmp(new tree('\0', 0));
		node = tmp;
	}

	huffman_d_read_tree(file, node->l);
	huffman_d_read_tree(file, node->r);
}

void huffman_d (const std::string& path)
{
	std::string data;

	/* read tree */
	boost::shared_ptr<tree> t;
	bit_file file(path);
	huffman_d_read_tree(file, t);

	if (!t)
		throw std::runtime_error("damaged file");

	try {
		boost::shared_ptr<tree> c = t;
		while (1) {
			if (file.pop())
				c = c->r;
			else
				c = c->l;
			if (!c)
				throw std::runtime_error("damaged file");
			if (!c->r && !c->l) {
				data += c->b;
				c = t;
			}
		}
	}
	catch (std::out_of_range& e) {
	}

	{
		std::ofstream file(path.c_str(), std::ios::binary | std::ios::out | std::ios::trunc);
		if (file.fail())
			throw std::runtime_error("could not open for writing");
		file.write(data.c_str(), data.length());
		file.close();
	}
}

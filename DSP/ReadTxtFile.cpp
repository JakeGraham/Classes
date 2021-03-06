#include<iostream>
#include<fstream>
#include<iterator>
#include<istream>
#include<sstream>
#include<iomanip>
#include<vector>



int main(){
	char infilename[100];

	std::cout << "Enter the address for the file you want to use" << std::endl;
	std::ifstream infile;
	std::cin.getline(infilename, 100);
	infile.open(infilename);	

	if (!infile.is_open()) {
		std::cout << "bad file" << std::endl;
	return(0);
	}

	else {
		std::cout << "File Opened Successfully" << std::endl;
	}

	std::vector<std::vector<double> > v1;

	std::string temp;

	// Imports data by line into 2d vector, each line is a column, number of columns = number of points in point cloud

	while (std::getline(infile, temp)) {
		std::istringstream buffer(temp);
		std::vector<double> line((std::istream_iterator<double>(buffer)),
			std::istream_iterator<double>());

		v1.push_back(line);
	}
	
	std::cout << v1[0].size() << std::endl;

	std::cout << v1[0][0] << std::endl;
	infile.close();
	return(1);
}

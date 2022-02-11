package main

import (
	"io/ioutil",
	"os"
)

func ParseFile() [][]int {
	filebuffer, err := ioutil.ReadFile("input.txt")
	if err != nil {
		os.Exit(1)
	}
	data := bufio.NewScanner(strings.NewReader(string(filebuffer)))
	data.Split(bufio.ScanRunes)
	matrix := make([][]int, 1)
	currentRow := 0

	for data.Scan() {
		i, err := strconv.Atoi(data.Text())
		if err == nil {
			matrix[currentRow] = append(matrix[currentRow], i)
		} else if data.Text() == "\n" {
			currentRow++
			matrix = append(matrix, make([]int, 0))
		}
	}
	return matrix
}

func main()  {

}
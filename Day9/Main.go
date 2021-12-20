package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
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

func FindLowPoints(matrix [][]int) int {
	riskSum := 0
	for r := range matrix {
		for c, v := range matrix[r] {
			isLowPoint := true
			if r != 0 {
				isLowPoint = isLowPoint && matrix[r-1][c] > v
			}
			if r < len(matrix)-1 {
				isLowPoint = isLowPoint && matrix[r+1][c] > v
			}
			if c != 0 {
				isLowPoint = isLowPoint && matrix[r][c-1] > v
			}
			if c < len(matrix[r])-1 {
				isLowPoint = isLowPoint && matrix[r][c+1] > v
			}
			// Add if this is low point
			if isLowPoint {
				riskSum += v + 1
			}
		}
	}
	return riskSum
}

func main() {
	matrix := ParseFile()
	risks := FindLowPoints(matrix)
	fmt.Println(risks)
}

package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Matrix = [5][5]int
type Marks = [5][5]bool

func FindInMatrix(m Matrix, toFind int) (int, int) {
	for r := range m {
		for c, v := range m[r] {
			if v == toFind {
				return r, c
			}
		}
	}
	return -1, -1
}

func ParseFile() ([]int, []Matrix, []Marks) {
	file, err := os.Open("input.txt")
	defer file.Close()
	if err != nil {
		panic(err)
	}
	var extraction []int
	var matrixes []Matrix
	var marks []Marks
	scanner := bufio.NewScanner(file)
	var currentMatrix Matrix
	currentRow := 0
	for i := 0; scanner.Scan(); i++ {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		if i == 0 {
			for _, v := range strings.Split(line, ",") {
				conv, _ := strconv.Atoi(v)
				extraction = append(extraction, conv)
			}
		} else {
			var converted [5]int
			for i, v := range strings.Fields(line) {
				conv, _ := strconv.Atoi(v)
				converted[i] = conv
			}
			currentMatrix[currentRow] = converted
			currentRow++
			if currentRow == 5 {
				matrixes = append(matrixes, currentMatrix)
				marks = append(marks, *new(Marks))
				currentRow = 0
			}
		}
	}

	return extraction, matrixes, marks
}

func HasWon(m Marks) bool {
	won := false
	for r := range m {
		won = m[r][0]
		for _, v := range m[r] {
			won = won && v
		}
		if won {
			return true
		}
	}
	for c := range m {
		won = m[0][c]
		for r := range m {
			won = m[r][c] && won
		}
		if won {
			return true
		}
	}
	return false
}

func Score(ma Matrix, marks Marks) int {
	score := 0
	for row := range ma {
		for col := range ma[row] {
			if !marks[row][col] {
				score += ma[row][col]
			}
		}
	}
	return score
}

func main() {
	extraction, matrixes, marks := ParseFile()
	alreadyWon := map[int]bool{}
	for _, v := range extraction {
		for i, m := range matrixes {
			row, col := FindInMatrix(m, v)
			if row != -1 {
				marks[i][row][col] = true
			}
		}
		for i, m := range marks {
			_, ok := alreadyWon[i]
			if ok {
				continue
			}
			if HasWon(m) {
				alreadyWon[i] = true
				fmt.Printf("Matrix %v has won with: %v\n", i, Score(matrixes[i], m)*v)
			}
		}
	}
}

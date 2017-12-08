package main

import (
	"bytes"
	"io"
	"io/ioutil"
	"log"
	"os"
	"path"
	"strings"
)

type timezone struct {
	Country string `json:"country"`
	Coords  string `json:"coords"`
	Region  string `json:"region"`
	City    string `json:"city"`
	Name    string `json:"name"`
	Comment string `json:"comment"`
}

func ParseTimezones() []timezone {
	filePath := path.Join(os.Getenv("TZDIR"), "zone.tab")
	fileBytes, err := ioutil.ReadFile(filePath)
	if err != nil {
		log.Fatal(err)
	}
	buf := bytes.NewBuffer(fileBytes)
	zones := []timezone{}
	for n := 0; n < 10000; n++ {
		line, err := buf.ReadString('\n')

		if err == io.EOF {
			break
		} else if err != nil {
			log.Fatal(err)
		}

		if strings.HasPrefix(line, "#") {
			continue
		}

		words := strings.Fields(line)
		regionCity := strings.Split(words[2], "/")
		comment := ""
		if len(words) >= 4 {
			comment = strings.Join(words[3:len(words)], " ")
		}
		timezone := timezone{
			Country: words[0],
			Coords:  words[1],
			Region:  regionCity[0],
			City:    regionCity[1],
			Name:    words[2],
			Comment: comment,
		}
		zones = append(zones, timezone)
	}

	return zones
}

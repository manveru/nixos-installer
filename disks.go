package main

import (
	"encoding/json"
	"log"
	"os/exec"
)

type lsblkDevice struct {
	Name       string        `json:"name"`
	KName      string        `json:"kname"`
	MajorMinor string        `json:"maj:min"`
	FSType     string        `json:"fstype"`
	MountPoint string        `json:"mountpoint"`
	Label      string        `json:"label"`
	UUID       string        `json:"uuid"`
	PartType   string        `json:"parttype"`
	PartLabel  string        `json:"partlabel"`
	PartUUID   string        `json:"partuuid"`
	PartFlags  string        `json:"partflags"`
	RA         string        `json:"ra"`
	RO         string        `json:"ro"`
	RM         string        `json:"rm"`
	HotPlug    string        `json:"hotplug"`
	Model      string        `json:"model"`
	Serial     string        `json:"serial"`
	Size       string        `json:"size"`
	State      string        `json:"state"`
	Owner      string        `json:"owner"`
	Disk       string        `json:"group"`
	Mode       string        `json:"mode"`
	Alignment  string        `json:"alignment"`
	MinIo      string        `json:"min-io"`
	OptIo      string        `json:"opt-io"`
	PhySec     string        `json:"phy-sec"`
	LogSec     string        `json:"log-sec"`
	Rota       string        `json:"rota"`
	Sched      string        `json:"sched"`
	RqSize     string        `json:"rq-size"`
	Type       string        `json:"type"`
	DiscAln    string        `json:"disc-aln"`
	DiscGran   string        `json:"disc-gran"`
	DiscMax    string        `json:"disc-max"`
	DiscZero   string        `json:"disc-zero"`
	WSAME      string        `json:"wsame"`
	WWN        string        `json:"wwn"`
	Rand       string        `json:"rand"`
	PKName     string        `json:"pkname"`
	HCTL       string        `json:"hctl"`
	Tran       string        `json:"tran"`
	Subsystems string        `json:"subsystems"`
	Rev        string        `json:"rev"`
	Vendor     string        `json:"vendor"`
	Zoned      string        `json:"zoned"`
	Children   []lsblkDevice `json:"children"`
}

type lsblkOutput struct {
	BlockDevices []lsblkDevice `json:"blockdevices"`
}

func ParseDisks() lsblkOutput {
	cmd := exec.Command("lsblk", "-O", "--json")
	out, err := cmd.CombinedOutput()
	if err != nil {
		log.Fatal(err)
	}
	v := lsblkOutput{}
	err = json.Unmarshal(out, &v)
	if err != nil {
		log.Fatal(err)
	}

	return v
}

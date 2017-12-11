import QtQuick 2.0;
import calamares.slideshow 1.0;

Presentation {
  id: presentation

  Timer {
    interval: 5000
    running: false
    repeat: true
    onTriggered: presentation.goToNextSlide()
  }

  Slide {
    Image {
      id: background
      source: "squid.png"
      width: 200; height: 200
      fillMode: Image.PreserveAspectFit
      anchors.centerIn: parent
    }
    Text {
      anchors.horizontalCenter: background.horizontalCenter
      anchors.top: background.bottom
      text: "This is a customizable QML slideshow.<br/>"+
            "Distributions should provide their own slideshow and list it in <br/>"+
            "their custom branding.desc file.<br/>"+
            "To create a Calamares presentation in QML, import calamares.slideshow,<br/>"+
            "define a Presentation element with as many Slide elements as needed."
      wrapMode: Text.WordWrap
      width: root.width
      horizontalAlignment: Text.Center
    }
  }

  Slide {
    centeredText: "This is a second Slide element."
  }

  Slide {
    centeredText: "This is a third Slide element."
  }
}
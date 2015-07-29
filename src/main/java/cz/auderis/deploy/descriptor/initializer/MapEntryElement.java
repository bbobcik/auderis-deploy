package cz.auderis.deploy.descriptor.initializer;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.io.Serializable;

@XmlRootElement(name = "entry")
@XmlType
public class MapEntryElement implements Serializable {
	private static final long serialVersionUID = 20150728L;

	@XmlElement(name = "key", required = true)
	protected MapKeyElement key;

	@XmlElement(name = "value", required = true)
	protected MapValueElement value;

}

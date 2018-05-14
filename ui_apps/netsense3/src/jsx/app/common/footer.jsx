export default class extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      version: "0.0.1"
    };
  }
  componentDidMount() {
    this.setState({
      version: document.getElementsByTagName('body')[0].getAttribute('data-version')
    });
  }
  render() {
    return (
      <div id='footer-container'>
        <Grid id='footer' className='text-center'>
          <Row>
            <Col xs={12}>
              <div>© 2016 Sensity Systems - v{this.state.version}</div>
            </Col>
          </Row>
        </Grid>
      </div>
    );
  }
}

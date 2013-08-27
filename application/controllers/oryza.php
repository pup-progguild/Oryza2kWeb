<?php
/**
 * Created by IntelliJ IDEA.
 * User: hoshi~
 * Date: 8/24/13
 * Time: 5:29 PM
 * To change this template use File | Settings | File Templates.
 */
class Oryza extends CI_Controller {
    private function _init() {
        $this->load->helper('url');
    }

    public function index() {
        $data['title'] = 'Home';

        $this->_init();

        $this->load->view('templates/header', $data);
        $this->load->view('templates/content-start', $data);
        $this->load->view('pages/home', $data);
        $this->load->view('templates/content-end', $data);
        $this->load->view('templates/footer', $data);
    }

    public function input() {
        $data['title'] = 'Input';

        $this->_init();

        $this->load->view('templates/header', $data);
        $this->load->view('templates/content-start', $data);
        $this->load->view('pages/input', $data);
        $this->load->view('templates/content-end', $data);
        $this->load->view('templates/input-script.js', $data);
        $this->load->view('templates/footer', $data);
    }

    public function about() {
        $data['title'] = 'About';

        $this->_init();

        $this->load->view('templates/header', $data);
        $this->load->view('templates/content-start', $data);
        $this->load->view('pages/about', $data);
        $this->load->view('templates/content-end', $data);
        $this->load->view('templates/footer', $data);
    }
}
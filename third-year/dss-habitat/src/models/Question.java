/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

/**
 *
 * @author joaorodrigues
 */
public class Question extends BasicModel {
    private Boolean enabled;
    private String text;

    public Question(String text, Boolean enabled) {
        super(-1);
        this.enabled = enabled;
        this.text = text;
    }

    public Question() {
        super();
    }
    
    public Boolean getEnabled() {
        return enabled;
    }

    public String getText() {
        return text;
    }

    public void setEnabled(Boolean enabled) {
        this.enabled = enabled;
    }

    public void setText(String text) {
        this.text = text;
    }
    
    @Override
    public String toString() {
        return this.text;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        
        if(obj == null || this.getClass() != obj.getClass())
            return false;
        
        Question q = (Question) obj;
        
        return ( super.equals(obj) );
    }
}
